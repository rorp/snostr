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

  it should "encode OK saved message" in {
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
      msg.result should be(a[OkRelayMessage.Saved])
      val result = msg.result.asInstanceOf[OkRelayMessage.Saved]
      result.duplicate should be(false)
      result.saved should be(true)
      result.message should be("saved")
      OkRelayMessage.Result.prefixedMessage(result) should be("saved")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }

  it should "encode OK duplicate message" in {

    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",true,"duplicate:"]"""

    val decoded = JsonDecoder[OkRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
      msg.saved should be(true)
      msg.message should be("duplicate:")
      msg.result should be(a[OkRelayMessage.Saved])
      val result = msg.result.asInstanceOf[OkRelayMessage.Saved]
      result.duplicate should be(true)
      result.saved should be(true)
      result.message should be("duplicate:")
      OkRelayMessage.Result.prefixedMessage(result) should be("duplicate:")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }

  it should "encode OK blocked message" in {

    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"blocked:"]"""

    val decoded = JsonDecoder[OkRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
      msg.saved should be(false)
      msg.message should be("blocked:")
      msg.result should be(a[OkRelayMessage.Blocked])
      val result = msg.result.asInstanceOf[OkRelayMessage.Blocked]
      result.saved should be(false)
      result.message should be("blocked:")
      OkRelayMessage.Result.prefixedMessage(result) should be("blocked:")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }

  it should "encode OK invalid message" in {

    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"invalid:"]"""

    val decoded = JsonDecoder[OkRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
      msg.saved should be(false)
      msg.message should be("invalid:")
      msg.result should be(a[OkRelayMessage.Invalid])
      val result = msg.result.asInstanceOf[OkRelayMessage.Invalid]
      result.saved should be(false)
      result.message should be("invalid:")
      OkRelayMessage.Result.prefixedMessage(result) should be("invalid:")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }

  it should "encode OK POW message" in {

    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"pow:"]"""

    val decoded = JsonDecoder[OkRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
      msg.saved should be(false)
      msg.message should be("pow:")
      msg.result should be(a[OkRelayMessage.Pow])
      val result = msg.result.asInstanceOf[OkRelayMessage.Pow]
      result.saved should be(false)
      result.message should be("pow:")
      OkRelayMessage.Result.prefixedMessage(result) should be("pow:")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }

  it should "encode OK rate limited message" in {

    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"rate-limited:"]"""

    val decoded = JsonDecoder[OkRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
      msg.saved should be(false)
      msg.message should be("rate-limited:")
      msg.result should be(a[OkRelayMessage.RateLimited])
      val result = msg.result.asInstanceOf[OkRelayMessage.RateLimited]
      result.saved should be(false)
      result.message should be("rate-limited:")
      OkRelayMessage.Result.prefixedMessage(result) should be("rate-limited:")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }

  it should "encode OK error message" in {

    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"error:"]"""

    val decoded = JsonDecoder[OkRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
      msg.saved should be(false)
      msg.message should be("error:")
      msg.result should be(a[OkRelayMessage.Error])
      val result = msg.result.asInstanceOf[OkRelayMessage.Error]
      result.saved should be(false)
      result.message should be("error:")
      OkRelayMessage.Result.prefixedMessage(result) should be("error:")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }

  it should "encode OK other rejected message" in {

    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"oops:"]"""

    val decoded = JsonDecoder[OkRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
      msg.saved should be(false)
      msg.message should be("oops:")
      msg.result should be(a[OkRelayMessage.Other])
      val result = msg.result.asInstanceOf[OkRelayMessage.Other]
      result.saved should be(false)
      result.message should be("oops:")
      OkRelayMessage.Result.prefixedMessage(result) should be("oops:")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }
}
