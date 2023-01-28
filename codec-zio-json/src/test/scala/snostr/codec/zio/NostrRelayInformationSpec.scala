package snostr.codec.zio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.core.{NostrPublicKey, NostrRelayInformation}
import snostr.codec.zio.JsonDecoders._
import snostr.codec.zio.JsonEncoders._
import zio.json.{EncoderOps, JsonDecoder}

class NostrRelayInformationSpec extends AnyFlatSpec with Matchers {
  it should "serialize/deserialize" in {
    {
      val sample = "{}"
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation())
      decoded.toJson should be(sample)
    }
    {
      val sample = """{"id":"12345"}"""
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation(
        id = Some("12345")))
      decoded.toJson should be(sample)
    }
    {
      val sample = """{"name":"relay name"}"""
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation(
        name = Some("relay name")))
      decoded.toJson should be(sample)
    }
    {
      val sample = """{"description":"relay desc"}"""
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation(
        description = Some("relay desc")))
      decoded.toJson should be(sample)
    }
    {
      val sample = """{"pubkey":"62e448d5a67cd9d24511d4024d687473b6f24663e91cd0d83d4b3f77566d0ed8"}"""
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation(
        pubkey = Some(NostrPublicKey.fromHex("62e448d5a67cd9d24511d4024d687473b6f24663e91cd0d83d4b3f77566d0ed8"))))
      decoded.toJson should be(sample)
    }
    {
      val sample = """{"pubkey":"malformed public key"}"""
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation())
      decoded.toJson should be("{}")
    }
    {
      val sample = """{"contact":"relay contact"}"""
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation(
        contact = Some("relay contact")))
      decoded.toJson should be(sample)
    }
    {
      val sample = """{"supported_nips":[1,2,4]}"""
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation(
        supportedNips = Vector(1, 2, 4)))
      decoded.toJson should be(sample)
    }
    {
      val sample = """{"software":"relay software"}"""
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation(
        software = Some("relay software")
      ))
      decoded.toJson should be(sample)
    }
    {
      val sample = """{"version":"relay version"}"""
      val Right(decoded) = JsonDecoder[NostrRelayInformation].decodeJson(sample)
      decoded should be(NostrRelayInformation(
        version = Some("relay version")
      ))
      decoded.toJson should be(sample)
    }
  }

  it should "parse RL relays information documents" in {
    val json1 = """{
                  |  "id": "wss://nostr.example.com/",
                  |  "name": "nostr-rs-relay",
                  |  "description": "A newly created nostr-rs-relay.\n\nCustomize this with your own info.",
                  |  "supported_nips": [
                  |    1,
                  |    2,
                  |    9,
                  |    11,
                  |    12,
                  |    15,
                  |    16,
                  |    20,
                  |    22
                  |  ],
                  |  "software": "https://git.sr.ht/~gheartsfield/nostr-rs-relay",
                  |  "version": "0.7.16"
                  |}
                  |""".stripMargin
    JsonDecoder[NostrRelayInformation].decodeJson(json1) should be(Right(NostrRelayInformation(
      id = Some("wss://nostr.example.com/"),
      name = Some("nostr-rs-relay"),
      description = Some("A newly created nostr-rs-relay.\n\nCustomize this with your own info."),
      supportedNips = Vector(1,2,9,11,12,15,16,20,22),
      software = Some("https://git.sr.ht/~gheartsfield/nostr-rs-relay"),
      version = Some("0.7.16")
    )))

    val json2 = """{"name":"nostream.your-domain.com","description":"A nostr relay written in TypeScript.","pubkey":"replace-with-your-pubkey","contact":"operator@your-domain.com","supported_nips":[1,2,4,9,11,12,15,16,20,22,26,28,33],"software":"git+https://github.com/Cameri/nostream.git","version":"1.16.0"}"""
    JsonDecoder[NostrRelayInformation].decodeJson(json2) should be(Right(NostrRelayInformation(
      name = Some("nostream.your-domain.com"),
      description = Some("A nostr relay written in TypeScript."),
      contact = Some("operator@your-domain.com"),
      supportedNips = Vector(1,2,4,9,11,12,15,16,20,22,26,28,33),
      software = Some("git+https://github.com/Cameri/nostream.git"),
      version = Some("1.16.0")
    )))
  }
}
