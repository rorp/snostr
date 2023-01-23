package snostr.codec.zio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.JsonDecoders._
import snostr.codec.zio.JsonEncoders._
import snostr.core.{EndOfStoredEventsRelayMessage, NoticeRelayMessage, OkRelayMessage, Sha256Digest}
import zio.json.{EncoderOps, JsonDecoder}

class NoticeRelayMessageSpec extends AnyFlatSpec with Matchers {
  it should "encode/decode NOTICE" in {
    JsonDecoder[NoticeRelayMessage].decodeJson("") should be(Left("Unexpected end of input"))
    JsonDecoder[NoticeRelayMessage].decodeJson("[]") should be(Left("[1](expected '\"' got ']')"))
    JsonDecoder[NoticeRelayMessage].decodeJson("""["NOTICE"]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[NoticeRelayMessage].decodeJson("""["NoTiCE","msg"]""") should be(Left("(unexpected message type: `NoTiCE`)"))
    JsonDecoder[NoticeRelayMessage].decodeJson("""["NOTICE","msg", ""]""") should be(Left("(expected ']' got ',')"))

    val json = """["NOTICE","this is a notice"]"""

    val decoded = JsonDecoder[NoticeRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.message should be("this is a notice")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }

  it should "encode/decode EOSE" in {
    JsonDecoder[EndOfStoredEventsRelayMessage].decodeJson("") should be(Left("Unexpected end of input"))
    JsonDecoder[EndOfStoredEventsRelayMessage].decodeJson("[]") should be(Left("[1](expected '\"' got ']')"))
    JsonDecoder[EndOfStoredEventsRelayMessage].decodeJson("""["EOSE"]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[EndOfStoredEventsRelayMessage].decodeJson("""["EoSe","msg"]""") should be(Left("(unexpected message type: `EoSe`)"))
    JsonDecoder[EndOfStoredEventsRelayMessage].decodeJson("""["EOSE","msg", ""]""") should be(Left("(expected ']' got ',')"))

    val json = """["EOSE","subscription id"]"""

    val decoded = JsonDecoder[EndOfStoredEventsRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.subscriptionId should be("subscription id")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }

  it should "encode OK" in {
    JsonDecoder[OkRelayMessage].decodeJson("") should be(Left("Unexpected end of input"))
    JsonDecoder[OkRelayMessage].decodeJson("[]") should be(Left("[1](expected '\"' got ']')"))
    JsonDecoder[OkRelayMessage].decodeJson("""["OK"]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[OkRelayMessage].decodeJson("""["OK","msg"]""") should be(Left("[2](Failed requirement.)"))
    JsonDecoder[OkRelayMessage].decodeJson("""["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00", ""]""") should be(Left("[3](expected 'true' or 'false' got \")"))
    JsonDecoder[OkRelayMessage].decodeJson("""["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00", true]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[OkRelayMessage].decodeJson("""["ok", "444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00", true, "msg"]""") should be(Left("(unexpected message type: `ok`)"))

    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",true,"saved"]"""

    val decoded = JsonDecoder[OkRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
      msg.saved should be(true)
      msg.message should be("saved")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }
}
