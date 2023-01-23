package snostr.codec.jackson

import com.fasterxml.jackson.databind.exc.MismatchedInputException
import org.json4s.MappingException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.jackson.JsonSerializers.{formats, serialization}
import snostr.core._

class NoticeRelayMessageSpec extends AnyFlatSpec with Matchers {
  it should "serialize/deserialize NOTICE" in {
    assertThrows[MismatchedInputException](serialization.read[NoticeRelayMessage](""))
    assertThrows[MappingException](serialization.read[NoticeRelayMessage]("[]"))
    assertThrows[MappingException](serialization.read[NoticeRelayMessage]("""["NOTICE"]"""))
    assertThrows[MappingException](serialization.read[NoticeRelayMessage]("""["NoTiCE","msg"]"""))
    assertThrows[MappingException](serialization.read[NoticeRelayMessage]("""["NOTICE","msg", ""]"""))

    val json = """["NOTICE","this is a notice"]"""

    val decoded = serialization.read[NoticeRelayMessage](json)

    decoded.message should be("this is a notice")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

  it should "serialize/deserialize EOSE" in {
    assertThrows[MismatchedInputException](serialization.read[EndOfStoredEventsRelayMessage](""))
    assertThrows[MappingException](serialization.read[EndOfStoredEventsRelayMessage]("[]"))
    assertThrows[MappingException](serialization.read[EndOfStoredEventsRelayMessage]("""["EOSE"]"""))
    assertThrows[MappingException](serialization.read[EndOfStoredEventsRelayMessage]("""["EoSe","msg"]"""))
    assertThrows[MappingException](serialization.read[EndOfStoredEventsRelayMessage]("""["EOSE","msg", ""]"""))

    val json = """["EOSE","subscription id"]"""

    val decoded = serialization.read[EndOfStoredEventsRelayMessage](json)

    decoded.subscriptionId should be("subscription id")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }


  it should "encode OK" in {
    assertThrows[MismatchedInputException](serialization.read[OkRelayMessage](""))
    assertThrows[MappingException](serialization.read[OkRelayMessage]("[]"))
    assertThrows[MappingException](serialization.read[OkRelayMessage]("""["OK"]"""))
    assertThrows[MappingException](serialization.read[OkRelayMessage]("""["OK","msg"]"""))
    assertThrows[MappingException](serialization.read[OkRelayMessage]("""["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00", ""]"""))
    assertThrows[MappingException](serialization.read[OkRelayMessage]("""["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00", true]"""))
    assertThrows[MappingException](serialization.read[OkRelayMessage]("""["ok", "444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00", true, "msg"]"""))

    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",true,"saved"]"""

    val decoded = serialization.read[OkRelayMessage](json)

    decoded.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
    decoded.saved should be(true)
    decoded.message should be("saved")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

}
