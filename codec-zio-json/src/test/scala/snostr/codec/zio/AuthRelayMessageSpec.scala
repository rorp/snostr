package snostr.codec.zio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.JsonDecoders._
import snostr.codec.zio.JsonEncoders._
import snostr.core.AuthRelayMessage
import zio.json.{EncoderOps, JsonDecoder}

class AuthRelayMessageSpec extends AnyFlatSpec with Matchers {
  it should "encode/decode AUTH" in {
    JsonDecoder[AuthRelayMessage].decodeJson("") should be(Left("Unexpected end of input"))
    JsonDecoder[AuthRelayMessage].decodeJson("[]") should be(Left("[1](expected '\"' got ']')"))
    JsonDecoder[AuthRelayMessage].decodeJson("""["AUTH"]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[AuthRelayMessage].decodeJson("""["AuTh","challenge"]""") should be(Left("(unexpected message type: `AuTh`)"))
    JsonDecoder[AuthRelayMessage].decodeJson("""["AUTH","challenge", ""]""") should be(Left("(expected ']' got ',')"))

    val json = """["AUTH","auth challenge"]"""

    val decoded = JsonDecoder[AuthRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.challenge should be("auth challenge")
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }
}
