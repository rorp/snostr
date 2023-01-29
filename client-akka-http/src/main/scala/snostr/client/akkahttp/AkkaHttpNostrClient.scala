package snostr.client.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding.Get
import akka.http.scaladsl.model.HttpHeader.ParsingResult.{Error, Ok}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.http.scaladsl.model.ws._
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, StatusCodes}
import akka.http.scaladsl.settings.{ClientConnectionSettings, ConnectionPoolSettings}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, Sink, Source}
import akka.util.ByteString
import io.github.rorp.akka.http.scaladsl.socks5.Socks5ClientTransport
import snostr.core._

import java.io.IOException
import java.net.{InetSocketAddress, URI}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

class AkkaHttpNostrClient(url: String,
                          username: Option[String] = None,
                          password: Option[String] = None,
                          socks5Proxy: Option[String] = None,
                          socks5Username: Option[String] = None,
                          socks5Password: Option[String] = None)(implicit codecs: Codecs, system: ActorSystem) extends NostrClient {

  import system.dispatcher

  private lazy val (queue, source) = Source
    .queue[Message](bufferSize = 128,
      OverflowStrategy.backpressure,
      maxConcurrentOffers = Math.max(Runtime.getRuntime.availableProcessors() / 2, 1))
    .toMat(BroadcastHub.sink)(Keep.both)
    .run()
  private lazy val sink = Sink.foreachAsync[Message](Runtime.getRuntime.availableProcessors()) {
    case TextMessage.Strict(text) =>
      Try(codecs.decodeRelayMessage(text)) match {
        case Success(message) =>
          Future.sequence(messageCallbacks.get().map(_.apply(message))).map(_ => ())
        case Failure(ex) =>
          Future.sequence(unknownMessageCallbacks.get().map(_.apply(text, ex))).map(_ => ())
      }
    case streamed: TextMessage.Streamed =>
      streamed.textStream.runWith(Sink.ignore)
      Future.unit
    case bm: BinaryMessage =>
      bm.dataStream.runWith(Sink.ignore)
      Future.unit
  }
  private lazy val basicFlow = Flow.fromSinkAndSourceMat(sink, source)(Keep.left)
  private lazy val wsFlow = basicFlow.watchTermination() { (_, termination) =>
    termination.onComplete { _ =>
      Try(disconnected.success(()))
    }
  }

  private val connected = Promise[Unit]()
  private val disconnected = Promise[Unit]()
  private val messageCallbacks = new AtomicReference[Vector[NostrRelayMessage => Future[Unit]]](Vector.empty)
  private val unknownMessageCallbacks = new AtomicReference[Vector[(String, Throwable) => Future[Unit]]](Vector.empty)

  override def connect(connectionTimeout: FiniteDuration = 60.seconds): Future[Unit] = {
    val settings = socks5ProxyTransport match {
      case Some(transport) =>
        ClientConnectionSettings(system).withTransport(transport)
      case None =>
        ClientConnectionSettings(system)
    }

    val upgradeResponse = Http().singleWebSocketRequest(
      WebSocketRequest(url, extraHeaders = authHeaders),
      clientFlow = wsFlow,
      settings = settings)._1

    upgradeResponse.foreach {
      case _: ValidUpgrade => connected.success(())
      case InvalidUpgradeResponse(response, cause) =>
        connected.failure(new IOException(s"Connection failed ${response.status}: $cause"))
    }

    akka.pattern.after(connectionTimeout, using = system.scheduler)(Future.failed(new IOException(s"Nostr client failed to connect after $connectionTimeout")))
      .recover { ex =>
        if (connected.tryFailure(ex)) {
          disconnected.trySuccess(())
          Try(queue.complete())
        }
      }

    connected.future
  }

  override def disconnect(): Future[Unit] = {
    checkConnected.flatMap { _ =>
      queue.complete()
      disconnected.future
    }
  }

  override def publish(event: NostrEvent): Future[Unit] = {
    checkConnected.flatMap(_ => sendClientMessage(EventClientMessage(event)))
  }

  override def subscribe(filters: Vector[NostrFilter], subscriptionId: String): Future[String] = {
    checkConnected.flatMap(_ => sendClientMessage(ReqClientMessage(subscriptionId, filters)).map(_ => subscriptionId))
  }

  private def sendClientMessage(message: NostrClientMessage): Future[Unit] = {
    val json = codecs.encodeClientMessage(message)
    queue.offer(TextMessage(json)).map(_ => ())
  }

  private def checkConnected: Future[Unit] = {
    if (!connected.isCompleted) {
      Future.failed(new RuntimeException("Nostr client is not connected"))
    } else {
      connected.future
    }
  }

  override def unsubscribe(subscriptionId: String): Future[Unit] = {
    checkConnected.flatMap(_ => sendClientMessage(CloseClientMessage(subscriptionId)))
  }

  override def addRelayMessageCallback(f: NostrRelayMessage => Future[Unit]): Future[Unit] = {
    Future.successful(messageCallbacks.updateAndGet((t: Vector[NostrRelayMessage => Future[Unit]]) => f +: t))
  }

  override def addUnknownRelayMessageCallback(f: (String, Throwable) => Future[Unit]): Future[Unit] = {
    Future.successful(unknownMessageCallbacks.updateAndGet((t: Vector[(String, Throwable) => Future[Unit]]) => f +: t))
  }

  override def relayInformation(extraHeaders: Vector[(String, String)] = Vector.empty): Future[NostrRelayInformation] = {
    val uri = if (url.startsWith("ws://")) {
      "http://" + url.drop(5)
    } else if (url.startsWith("wss://")) {
      "https://" + url.drop(6)
    } else url

    def header(name: String, value: String): HttpHeader = {
      HttpHeader.parse(name, value) match {
        case Ok(header, _) => header
        case Error(error) => throw new IllegalArgumentException(error.formatPretty)
      }
    }

    val headers = header("Accept", "application/nostr+json") ::
      extraHeaders.map(h => header(h._1, h._2)).toList ++
        authHeaders

    val settings = socks5ProxyTransport match {
      case Some(transport) =>
        ConnectionPoolSettings(system).withTransport(transport)
      case None =>
        ConnectionPoolSettings(system)
    }

    val req: HttpRequest = headers.foldLeft(Get(uri))((acc, x) => acc.addHeader(x))

    for {
      res <- Http().singleRequest(req, settings = settings)
      body <- res.status match {
        case _: StatusCodes.Success =>
          res.entity.dataBytes.runFold(ByteString(""))(_ ++ _)
        case e@(StatusCodes.ServerError(_) | StatusCodes.ClientError(_)) =>
          throw new IOException(s"relay information request failed: ${e.intValue} ${e.reason}")
        case other =>
          throw new IOException(s"relay returned ${other.intValue} ${other.reason}")
      }
    } yield {
      codecs.decodeRelayInfo(body.utf8String)
    }
  }

  private def authHeaders = (for {
    u <- username
    p <- password
  } yield Seq(Authorization(BasicHttpCredentials(u, p)))).getOrElse(Nil)

  private def socks5ProxyTransport = socks5Proxy.map { url =>
    val uri = new URI(url)

    val proxyAddress = InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)

    val socks5Credentials = for {
      username <- socks5Username
      password <- socks5Password
    } yield BasicHttpCredentials(username, password)

    socks5Credentials match {
      case Some(proxyAuth) => Socks5ClientTransport.socks5Proxy(proxyAddress, proxyAuth)
      case None => Socks5ClientTransport.socks5Proxy(proxyAddress)
    }
  }
}
