package snostr.client.akkahttp

import akka.actor.ActorSystem
import akka.testkit.TestKit.awaitCond
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.ZioJsonCodecs
import snostr.core._

import java.net.{InetSocketAddress, Socket, URI}
import java.time.Instant
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Try

class AkkaHttpNostrClientSocks5Spec extends AsyncFlatSpec with Matchers {

  implicit val system = ActorSystem()
  implicit val ec = system.dispatcher
  implicit val codecs = ZioJsonCodecs

  val url = "wss://nostr.zebedee.cloud:443"
  val socks5Url = "socks5://localhost:9050"

  it should "post and receive messages with zio-json via SOCKS5 proxy" in {
    assume(remoteHostIsListening(socks5Url), "the SOCKS5 proxy is unreachable")
    assume(remoteHostIsListening(url), "the relay is unreachable")

    val client = new AkkaHttpNostrClient(url, socks5Proxy = Some(socks5Url))

    var receivedMessages = Vector.empty[NostrRelayMessage]
    client.addRelayMessageCallback(msg => Future.successful {
      receivedMessages = receivedMessages :+ msg
    })

    var unknownMessages = Vector.empty[(String, Throwable)]
    client.addUnknownRelayMessageCallback((msg, ex) => Future.successful {
      unknownMessages = unknownMessages :+ (msg, ex)
    })

    val seckey = NostrPrivateKey.freshPrivateKey
    // remove all possible nanoseconds from the timestamp
    val createdAt = Instant.ofEpochSecond(Instant.now().getEpochSecond)
    val event = NostrEvent.textNote(seckey, content = "test", createdAt = createdAt)
    val filter = NostrFilter()
      .withAuthors(Vector(seckey.publicKey.toHex))
      .witKinds(Vector(0, 1, 2, 3, 4))
      .withLimit(10)

    for {
      _ <- client.connect()
      _ <- client.publish(event)
      _ = awaitCond(receivedMessages.exists(x => x.isInstanceOf[OkRelayMessage]), 30.seconds)
      _ <- client.subscribe(Vector(filter), "abc")
      _ <- client.subscribe(Vector.empty, "xyz")
      _ = awaitCond(receivedMessages.size == 4, 30.seconds)
      _ <- client.disconnect()
    } yield {
      val em = receivedMessages.collect { case e: EventRelayMessage => e }
      em shouldNot be(empty)
      em.foreach { receivedEvent =>
        receivedEvent.subscriptionId should be("abc")
        receivedEvent.event.validId should be(true)
        receivedEvent.event.validSignature should be(true)
        receivedEvent.event should be(event)
      }

      receivedMessages.contains(NoticeRelayMessage("invalid: \"REQ message\" does not contain [filter]"))
      receivedMessages.contains(EndOfStoredEventsRelayMessage("abc"))
      receivedMessages.contains(OkRelayMessage(event.id, saved = true, message = ""))


      unknownMessages.size should be(0)
    }
  }

  def remoteHostIsListening(url: String): Boolean = {
    val uri = new URI(url)
    remoteHostIsListening(InetSocketAddress.createUnresolved(uri.getHost, uri.getPort))
  }

  def remoteHostIsListening(address: InetSocketAddress): Boolean =
    Try {
      val socket = new Socket(address.getHostString, address.getPort)
      socket.close()
    }.isSuccess

}