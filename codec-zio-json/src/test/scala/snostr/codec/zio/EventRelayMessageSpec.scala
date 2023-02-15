package snostr.codec.zio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.JsonDecoders._
import snostr.codec.zio.JsonEncoders._
import snostr.core._
import zio.json.{EncoderOps, JsonDecoder}

class EventRelayMessageSpec extends AnyFlatSpec with Matchers {
  it should "encode/decode EVENT" in {
    JsonDecoder[EventRelayMessage].decodeJson("") should be(Left("Unexpected end of input"))
    JsonDecoder[EventRelayMessage].decodeJson("[]") should be(Left("""[1](expected '"' got ']')"""))
    JsonDecoder[EventRelayMessage].decodeJson("""["EVENT"]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[EventRelayMessage].decodeJson("""["EVENT","subid"]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[EventRelayMessage].decodeJson("""["eVENt","subid",{"id":"07903a662720a33dc334544687ca4d460d7ea5b3d28e978ecc06ecaf4c17747e","pubkey":"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at":1671663042,"kind":1,"tags":[["e","a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"]],"content":"αυτό είναι ένα μήνυμα","sig":"a3b66bfad7ee596bc01aa85cb797493982e43525fb70d3b30b46c3814b72213ff3efcdb2e38a12d0cba18ae4e7b8288f446233fa95bd5086a6b1a1bb7f5bb26a"}]""") should be(Left("(unexpected message type: `eVENt`)"))
    JsonDecoder[EventRelayMessage].decodeJson("""["EVENT","subid",{"id":"07903a662720a33dc334544687ca4d460d7ea5b3d28e978ecc06ecaf4c17747e","pubkey":"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at":1671663042,"kind":1,"tags":[["e","a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"]],"content":"αυτό είναι ένα μήνυμα","sig":"a3b66bfad7ee596bc01aa85cb797493982e43525fb70d3b30b46c3814b72213ff3efcdb2e38a12d0cba18ae4e7b8288f446233fa95bd5086a6b1a1bb7f5bb26a"},"extra"]""") should be(Left("(expected ']' got ',')"))

    val json = """["EVENT","subscription003",{"id":"07903a662720a33dc334544687ca4d460d7ea5b3d28e978ecc06ecaf4c17747e","pubkey":"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at":1671663042,"kind":1,"tags":[["e","a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"]],"content":"αυτό είναι ένα μήνυμα","sig":"a3b66bfad7ee596bc01aa85cb797493982e43525fb70d3b30b46c3814b72213ff3efcdb2e38a12d0cba18ae4e7b8288f446233fa95bd5086a6b1a1bb7f5bb26a"}]"""

    val decoded = JsonDecoder[EventRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.subscriptionId should be("subscription003")
      msg.event.id should be(Sha256Digest.fromHex("07903a662720a33dc334544687ca4d460d7ea5b3d28e978ecc06ecaf4c17747e"))
      msg.event.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
      msg.event.sig should be(NostrSignature.fromHex("a3b66bfad7ee596bc01aa85cb797493982e43525fb70d3b30b46c3814b72213ff3efcdb2e38a12d0cba18ae4e7b8288f446233fa95bd5086a6b1a1bb7f5bb26a"))
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }
}
