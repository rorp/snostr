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


  it should "encode OK saved message" in {
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
    decoded.result should be(a[OkRelayMessage.Saved])
    val result = decoded.result.asInstanceOf[OkRelayMessage.Saved]
    result.saved should be(true)
    result.duplicate should be(false)
    result.message should be("saved")
    OkRelayMessage.Result.prefixedMessage(result) should be("saved")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

  it should "encode OK duplicate message" in {
    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",true,"duplicate:saved"]"""

    val decoded = serialization.read[OkRelayMessage](json)

    decoded.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
    decoded.saved should be(true)
    decoded.message should be("duplicate:saved")
    decoded.result should be(a[OkRelayMessage.Saved])
    val result = decoded.result.asInstanceOf[OkRelayMessage.Saved]
    result.saved should be(true)
    result.duplicate should be(true)
    result.message should be("duplicate:saved")
    OkRelayMessage.Result.prefixedMessage(result) should be("duplicate:saved")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

  it should "encode OK blocked message" in {
    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"blocked:"]"""

    val decoded = serialization.read[OkRelayMessage](json)

    decoded.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
    decoded.saved should be(false)
    decoded.message should be("blocked:")
    decoded.result should be(a[OkRelayMessage.Blocked])
    val result = decoded.result.asInstanceOf[OkRelayMessage.Blocked]
    result.saved should be(false)
    result.message should be("blocked:")
    OkRelayMessage.Result.prefixedMessage(result) should be("blocked:")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

  it should "encode OK invalid message" in {
    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"invalid:"]"""

    val decoded = serialization.read[OkRelayMessage](json)

    decoded.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
    decoded.saved should be(false)
    decoded.message should be("invalid:")
    decoded.result should be(a[OkRelayMessage.Invalid])
    val result = decoded.result.asInstanceOf[OkRelayMessage.Invalid]
    result.saved should be(false)
    result.message should be("invalid:")
    OkRelayMessage.Result.prefixedMessage(result) should be("invalid:")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

  it should "encode OK POW message" in {
    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"pow:"]"""

    val decoded = serialization.read[OkRelayMessage](json)

    decoded.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
    decoded.saved should be(false)
    decoded.message should be("pow:")
    decoded.result should be(a[OkRelayMessage.Pow])
    val result = decoded.result.asInstanceOf[OkRelayMessage.Pow]
    result.saved should be(false)
    result.message should be("pow:")
    OkRelayMessage.Result.prefixedMessage(result) should be("pow:")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

  it should "encode OK rate limited message" in {
    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"rate-limited:"]"""

    val decoded = serialization.read[OkRelayMessage](json)

    decoded.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
    decoded.saved should be(false)
    decoded.message should be("rate-limited:")
    decoded.result should be(a[OkRelayMessage.RateLimited])
    val result = decoded.result.asInstanceOf[OkRelayMessage.RateLimited]
    result.saved should be(false)
    result.message should be("rate-limited:")
    OkRelayMessage.Result.prefixedMessage(result) should be("rate-limited:")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

  it should "encode OK Error message" in {
    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"error:"]"""

    val decoded = serialization.read[OkRelayMessage](json)

    decoded.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
    decoded.saved should be(false)
    decoded.message should be("error:")
    decoded.result should be(a[OkRelayMessage.Error])
    val result = decoded.result.asInstanceOf[OkRelayMessage.Error]
    result.saved should be(false)
    result.message should be("error:")
    OkRelayMessage.Result.prefixedMessage(result) should be("error:")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

  it should "encode OK Restricted message" in {
    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"restricted: we do not accept events from unauthenticated users"]"""

    val decoded = serialization.read[OkRelayMessage](json)

    decoded.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
    decoded.saved should be(false)
    decoded.message should be("restricted: we do not accept events from unauthenticated users")
    decoded.result should be(a[OkRelayMessage.Restricted])
    val result = decoded.result.asInstanceOf[OkRelayMessage.Restricted]
    result.saved should be(false)
    result.message should be("restricted: we do not accept events from unauthenticated users")
    OkRelayMessage.Result.prefixedMessage(result) should be("restricted: we do not accept events from unauthenticated users")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }

  it should "encode OK other rejected message" in {
    val json = """["OK","444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00",false,"opps:"]"""

    val decoded = serialization.read[OkRelayMessage](json)

    decoded.eventId should be(Sha256Digest.fromHex("444a130faf1757b11d0cb9ab6d24da0a2d001ea849e091b646de9d24ee05be00"))
    decoded.saved should be(false)
    decoded.message should be("opps:")
    decoded.result should be(a[OkRelayMessage.Other])
    val result = decoded.result.asInstanceOf[OkRelayMessage.Other]
    result.saved should be(false)
    result.message should be("opps:")
    OkRelayMessage.Result.prefixedMessage(result) should be("opps:")

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeRelayMessage(encoded) should be(decoded)
    JacksonCodecs.encodeRelayMessage(decoded) should be(json)
  }
}
