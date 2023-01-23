package snostr.codec.jackson

import com.fasterxml.jackson.databind.exc.MismatchedInputException
import org.json4s.MappingException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.jackson.JsonSerializers.{formats, serialization}
import snostr.core._

class ReqClientMessageSpec extends AnyFlatSpec with Matchers {
  it should "serialize/deserialize" in {
    assertThrows[MismatchedInputException](serialization.read[ReqClientMessage](""))
    assertThrows[MappingException](serialization.read[ReqClientMessage]("[]"))
    assertThrows[MappingException](serialization.read[ReqClientMessage]("""["REQ"]"""))
    assertThrows[MappingException](serialization.read[ReqClientMessage]("""["REQ","subid"]"""))
    assertThrows[MappingException](serialization.read[ReqClientMessage]("""["REQ","subid", ""]"""))

    val msg = serialization.read[ReqClientMessage]("""["REQ","subid", {}, {}]""")
    msg.filters should be(Vector(
      NostrFilter(),
      NostrFilter()))

    val json = """["REQ","subscription0002",{"ids":["418099875e13998f3ebac0a6d76725c23816cf4b50e8042399c0fceb9a69212c"],"authors":["7f4d5914d058b8f1afeb3dcb18fb61d7689d2088f48a846f04444073dfca7a7f"],"kinds":[1,2,3],"#e":["fd6c5b10c56db9a953cb8c9d44062fb2b676d22fa8277df8d65494eb061f6740"],"#p":["6dac3158ea021bcec4189f30730a9747befdc8b575ecb89026bd4a36aadf4450"],"until":10000,"limit":10},{"authors":["7f4d5914d058b8f1afeb3dcb18fb61d7689d2088f48a846f04444073dfca7a7f","6dac3158ea021bcec4189f30730a9747befdc8b575ecb89026bd4a36aadf4450"],"limit":20}]"""

    val decoded = serialization.read[ReqClientMessage](json)

    decoded.subscriptionId should be("subscription0002")
    decoded.filters.length should be(2)
    decoded.filters(0).ids should be(Vector("418099875e13998f3ebac0a6d76725c23816cf4b50e8042399c0fceb9a69212c"))
    decoded.filters(0).authors should be(Vector("7f4d5914d058b8f1afeb3dcb18fb61d7689d2088f48a846f04444073dfca7a7f"))
    decoded.filters(0).limit should be(Some(10))
    decoded.filters(1).ids should be(Vector())
    decoded.filters(1).authors should be(Vector("7f4d5914d058b8f1afeb3dcb18fb61d7689d2088f48a846f04444073dfca7a7f", "6dac3158ea021bcec4189f30730a9747befdc8b575ecb89026bd4a36aadf4450"))
    decoded.filters(1).limit should be(Some(20))

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeClientMessage(encoded) should be(decoded)
    JacksonCodecs.encodeClientMessage(decoded) should be(json)
  }
}
