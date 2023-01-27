package snostr.client.akkahttp

import akka.actor.ActorSystem
import akka.testkit.TestKit.awaitCond
import com.dimafeng.testcontainers.{ForAllTestContainer, GenericContainer}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.testcontainers.containers.wait.strategy.Wait
import snostr.codec.jackson.JacksonCodecs
import snostr.codec.zio.ZioJsonCodecs
import snostr.core.OkRelayMessage.Saved
import snostr.core._

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class AkkaHttpNostrClientSpec extends AsyncFlatSpec with Matchers with ForAllTestContainer {

  implicit val system = ActorSystem()
  implicit val ec = system.dispatcher

  override val container =
    GenericContainer(
      "scsibug/nostr-rs-relay:latest",
      exposedPorts = Seq(8080),
      waitStrategy = Wait.forListeningPort())

  it should "post and receive messages with zio-json" in {
    testPostAndRecieve(ZioJsonCodecs)
  }

  it should "post and receive messages with jackson" in {
    testPostAndRecieve(JacksonCodecs)
  }

  private def testPostAndRecieve(implicit codecs: Codecs) = {
    val url = s"ws://${container.containerIpAddress}:${container.mappedPort(8080)}"

    val client = new AkkaHttpNostrClient(url)

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
      _ <- client.connect()
      _ <- client.publish(event)
      _ = awaitCond(receivedMessages.exists(x => x.isInstanceOf[OkRelayMessage]), 30.seconds)
      _ <- client.subscribe(Vector(filter), "abc")
      _ <- client.subscribe(Vector.empty, "xyz")
      _ <- client.publish(event)
      _ = awaitCond(receivedMessages.size == 5, 30.seconds)
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

      receivedMessages.contains(NoticeRelayMessage("could not parse command")) should be(true)
      receivedMessages.contains(EndOfStoredEventsRelayMessage("abc")) should be(true)
      receivedMessages.contains(OkRelayMessage(event.id, Saved(message = ""))) should be(true)
      receivedMessages.contains(OkRelayMessage(event.id, Saved(message = "duplicate: "))) should be(true)

      unknownMessages should be(empty)
    }
  }
}