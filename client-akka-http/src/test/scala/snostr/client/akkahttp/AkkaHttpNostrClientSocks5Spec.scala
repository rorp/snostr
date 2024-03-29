package snostr.client.akkahttp

import akka.actor.ActorSystem
import akka.testkit.TestKit.awaitCond
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.ZioJsonCodecs
import snostr.core.OkRelayMessage.Saved
import snostr.core._

import java.net.{InetSocketAddress, Socket, URI}
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Try

class AkkaHttpNostrClientSocks5Spec extends AsyncFlatSpec with Matchers {

  implicit val system = ActorSystem()
  implicit val ec = system.dispatcher
  implicit val codecs = ZioJsonCodecs

  val url = "wss://relay.exit.pub:443"
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
    val expiration = Instant.ofEpochSecond(Instant.now().plus(15, ChronoUnit.MINUTES).getEpochSecond)
    val event = NostrEvent.textNote(seckey, content = "test", createdAt = createdAt, expiration = Some(expiration))
    val filter = NostrFilter()
      .withAuthors(Vector(seckey.publicKey.toHex))
      .witKinds(Vector(0, 1, 2, 3, 4))
      .withLimit(10)

    for {
      info <- client.relayInformation()
      _ <- client.connect()
      _ <- client.publish(event)
      _ = awaitCond(receivedMessages.exists(x => x.isInstanceOf[OkRelayMessage]), 30.seconds)
      _ <- client.subscribe(Vector(filter), "abc")
      _ <- client.subscribe(Vector.empty, "xyz")
      _ <- client.publish(event)
      _ = awaitCond(receivedMessages.size == 5, 30.seconds)
      _ <- client.disconnect()
    } yield {
      info should be(NostrRelayInformation(
        id = None,
        name = Some("EXIT.pub relay"),
        description = Some("Exit.pub relay"),
        pubkey = Some(NostrPublicKey.fromHex("fa984bd7dbb282f07e16e7ae87b26a2a7b9b90b7246a44771f0cf5ae58018f52")),
        contact = Some("https://pablof7z.com"),
        supportedNips = Vector(1, 2, 4, 9, 11, 12, 16, 20, 22, 28, 33, 40),
        software = Some("git+https://github.com/hoytech/strfry.git"),
        version = Some("0.9.6-2-g3579f21")))

      info.supports(1, 2) should be(true)

      val em = receivedMessages.collect { case e: EventRelayMessage => e }
      em shouldNot be(empty)
      em.foreach { receivedEvent =>
        receivedEvent.subscriptionId should be("abc")
        receivedEvent.event.validId should be(true)
        receivedEvent.event.validSignature should be(true)
        receivedEvent.event should be(event)
      }

      receivedMessages.contains(NoticeRelayMessage("ERROR: bad req: arr too small")) should be(true)
      receivedMessages.contains(EndOfStoredEventsRelayMessage("abc")) should be(true)
      receivedMessages.contains(OkRelayMessage(event.id, Saved(message = ""))) should be(true)
      receivedMessages.contains(OkRelayMessage(event.id, Saved(message = "duplicate: have this event"))) should be(true)

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