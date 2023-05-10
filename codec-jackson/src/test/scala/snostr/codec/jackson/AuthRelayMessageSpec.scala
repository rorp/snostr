package snostr.codec.jackson

import com.fasterxml.jackson.databind.exc.MismatchedInputException
import org.json4s.MappingException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.jackson.JsonSerializers.{formats, serialization}
import snostr.core.AuthRelayMessage

class AuthRelayMessageSpec extends AnyFlatSpec with Matchers {
  it should "serialize/deserialize AUTH" in {
    assertThrows[MismatchedInputException](serialization.read[AuthRelayMessage](""))
    assertThrows[MappingException](serialization.read[AuthRelayMessage]("[]"))
    assertThrows[MappingException](serialization.read[AuthRelayMessage]("""["AUTH"]"""))
    assertThrows[MappingException](serialization.read[AuthRelayMessage]("""["AuTh","msg"]"""))
    assertThrows[MappingException](serialization.read[AuthRelayMessage]("""["AUTH","msg", ""]"""))

    val json = """["AUTH","auth challenge"]"""

    val decoded = serialization.read[AuthRelayMessage](json)

    decoded.challenge should be("auth challenge")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }
}
