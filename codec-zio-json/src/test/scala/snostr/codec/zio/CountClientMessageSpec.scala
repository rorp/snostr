package snostr.codec.zio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.JsonDecoders._
import snostr.codec.zio.JsonEncoders._
import snostr.core._
import zio.json.{EncoderOps, JsonDecoder}

class CountClientMessageSpec extends AnyFlatSpec with Matchers {
  it should "encode/decode COUNT" in {
    JsonDecoder[CountClientMessage].decodeJson("") should be(Left("Unexpected end of input"))
    JsonDecoder[CountClientMessage].decodeJson("[]") should be(Left("(invalid COUNT message)"))
    JsonDecoder[CountClientMessage].decodeJson("""["COUNT"]""") should be(Left("(invalid COUNT message)"))
    JsonDecoder[CountClientMessage].decodeJson("""["COUNT","subid"]""") should be(Left("(invalid COUNT message)"))
    JsonDecoder[CountClientMessage].decodeJson("""["COUNT","subid", ""]""") should be(Left("([0](expected '{' got '\"'))"))

    val Right(msg) = JsonDecoder[CountClientMessage].decodeJson("""["COUNT","subid", {}, {}]""")
    msg.filters should be(Vector(
      NostrFilter(),
      NostrFilter()))

    val json = """["COUNT","subscription0002",{"ids":["418099875e13998f3ebac0a6d76725c23816cf4b50e8042399c0fceb9a69212c"],"authors":["7f4d5914d058b8f1afeb3dcb18fb61d7689d2088f48a846f04444073dfca7a7f"],"kinds":[1,2,3],"#e":["fd6c5b10c56db9a953cb8c9d44062fb2b676d22fa8277df8d65494eb061f6740"],"#p":["6dac3158ea021bcec4189f30730a9747befdc8b575ecb89026bd4a36aadf4450"],"until":10000,"limit":10},{"authors":["7f4d5914d058b8f1afeb3dcb18fb61d7689d2088f48a846f04444073dfca7a7f","6dac3158ea021bcec4189f30730a9747befdc8b575ecb89026bd4a36aadf4450"],"limit":20}]"""

    val decoded = JsonDecoder[CountClientMessage].decodeJson(json)

    decoded.isRight should be(true)
    decoded.foreach { msg =>
      msg.subscriptionId should be("subscription0002")
      msg.filters.length should be(2)
      msg.filters(0).ids should be(Vector("418099875e13998f3ebac0a6d76725c23816cf4b50e8042399c0fceb9a69212c"))
      msg.filters(0).authors should be(Vector("7f4d5914d058b8f1afeb3dcb18fb61d7689d2088f48a846f04444073dfca7a7f"))
      msg.filters(0).limit should be(Some(10))
      msg.filters(1).ids should be(Vector())
      msg.filters(1).authors should be(Vector("7f4d5914d058b8f1afeb3dcb18fb61d7689d2088f48a846f04444073dfca7a7f", "6dac3158ea021bcec4189f30730a9747befdc8b575ecb89026bd4a36aadf4450"))
      msg.filters(1).limit should be(Some(20))
    }

    val encoded = decoded.getOrElse(throw new RuntimeException()).toJson

    encoded should be(json)

    ZioJsonCodecs.encodeClientMessage(decoded.toOption.get) should be(encoded)
    ZioJsonCodecs.decodeClientMessage(encoded) should be(decoded.toOption.get)
  }
}
