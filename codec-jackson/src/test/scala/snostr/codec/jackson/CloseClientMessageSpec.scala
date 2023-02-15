package snostr.codec.jackson

import com.fasterxml.jackson.databind.exc.MismatchedInputException
import org.json4s.MappingException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.jackson.JsonSerializers.{formats, serialization}
import snostr.core._

class CloseClientMessageSpec extends AnyFlatSpec with Matchers {
  it should "serialize/deserialize CLOSE" in {
    assertThrows[MismatchedInputException](serialization.read[CloseClientMessage](""))
    assertThrows[MappingException](serialization.read[CloseClientMessage]("[]"))
    assertThrows[MappingException](serialization.read[CloseClientMessage]("""["CLOSE"]"""))
    assertThrows[MappingException](serialization.read[CloseClientMessage]("""["cLOSe","subid"]"""))
    assertThrows[MappingException](serialization.read[CloseClientMessage]("""["CLOSE","subid", "extra"]"""))

    val json = """["CLOSE","subscription0001"]"""

    val decoded = serialization.read[CloseClientMessage](json)
    decoded.subscriptionId should be("subscription0001")

    val encoded = serialization.write[CloseClientMessage](decoded)
    encoded should be(json)

    JacksonCodecs.decodeClientMessage(encoded) should be(decoded)
    JacksonCodecs.encodeClientMessage(decoded) should be(json)
  }
}
