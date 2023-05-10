package snostr.codec.zio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.JsonDecoders._
import snostr.codec.zio.JsonEncoders._
import snostr.core._
import zio.json.{EncoderOps, JsonDecoder}

class CountRelayMessageSpec extends AnyFlatSpec with Matchers {
  it should "encode/decode COUNT" in {
    JsonDecoder[CountRelayMessage].decodeJson("") should be(Left("Unexpected end of input"))
    JsonDecoder[CountRelayMessage].decodeJson("[]") should be(Left("""[1](expected '"' got ']')"""))
    JsonDecoder[CountRelayMessage].decodeJson("""["COUNT"]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[CountRelayMessage].decodeJson("""["COUNT","subid"]""") should be(Left("(expected ',' got ']')"))
    JsonDecoder[CountRelayMessage].decodeJson("""["cOUNt","subid",{"count":12345}]""") should be(Left("(unexpected message type: `cOUNt`)"))
    JsonDecoder[CountRelayMessage].decodeJson("""["COUNT","subid",{"count":12345},"extra"]""") should be(Left("(expected ']' got ',')"))
    JsonDecoder[CountRelayMessage].decodeJson("""["COUNT","subid",{"cOUNt":12345}]""") should be(Left("(key not found: count)"))

    val json = """["COUNT","subscription003",{"count":12345}]"""

    val decoded = JsonDecoder[CountRelayMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.subscriptionId should be("subscription003")
      msg.count should be(12345)
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeRelayMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeRelayMessage(encoded) should be(decoded.toOption.get)
  }
}
