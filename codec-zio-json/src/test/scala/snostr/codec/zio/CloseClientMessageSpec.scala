package snostr.codec.zio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.JsonDecoders._
import snostr.codec.zio.JsonEncoders._
import snostr.core._
import zio.json.{EncoderOps, JsonDecoder}

class CloseClientMessageSpec extends AnyFlatSpec with Matchers {
  it should "encode/decode" in {
    JsonDecoder[CloseClientMessage].decodeJson("") should be(Left("Unexpected end of input"))
    JsonDecoder[CloseClientMessage].decodeJson("[]") should be(Left("""[1](expected '"' got ']')"""))
    JsonDecoder[CloseClientMessage].decodeJson("""["CLOSE"]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[CloseClientMessage].decodeJson("""["cLOSe","subid"]""") should be(Left("(unexpected message type: `cLOSe`)"))
    JsonDecoder[CloseClientMessage].decodeJson("""["CLOSE","subid", "extra"]""") should be(Left("(expected ']' got ',')"))

    val json = """["CLOSE","subscription0001"]"""

    val decoded = JsonDecoder[CloseClientMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.subscriptionId should be("subscription0001")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeClientMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeClientMessage(encoded) should be(decoded.toOption.get)
  }
}
