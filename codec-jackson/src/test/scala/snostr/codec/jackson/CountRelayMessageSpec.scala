package snostr.codec.jackson

import com.fasterxml.jackson.databind.exc.MismatchedInputException
import org.json4s.MappingException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.jackson.JsonSerializers.{formats, serialization}
import snostr.core.CountRelayMessage

class CountRelayMessageSpec extends AnyFlatSpec with Matchers {
  it should "serialize/deserialize COUNT" in {
    assertThrows[MismatchedInputException](serialization.read[CountRelayMessage](""))
    assertThrows[MappingException](serialization.read[CountRelayMessage]("[]"))
    assertThrows[MappingException](serialization.read[CountRelayMessage]("""["COUNT"]"""))
    assertThrows[MappingException](serialization.read[CountRelayMessage]("""["COUNT","subid"]"""))
    assertThrows[MappingException](serialization.read[CountRelayMessage]("""["cOUNt","subid",{"count":12345}]"""))
    assertThrows[MappingException](serialization.read[CountRelayMessage]("""["COUNT","subid",{"count":12345},"extra"]"""))
    assertThrows[MappingException](serialization.read[CountRelayMessage]("""["COUNT","subid",{"cOUNt":12345}]"""))

    val json = """["COUNT","subscription003",{"count":12345}]"""

    val decoded = serialization.read[CountRelayMessage](json)

    decoded.subscriptionId should be("subscription003")
    decoded.count should be(12345)

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }
}
