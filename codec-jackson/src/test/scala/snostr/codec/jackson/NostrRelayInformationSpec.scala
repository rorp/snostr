package snostr.codec.jackson

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.core.{NostrPublicKey, NostrRelayInformation}
import snostr.codec.jackson.JsonSerializers.{formats, serialization}

class NostrRelayInformationSpec  extends AnyFlatSpec with Matchers {
  it should "serialize/deserialize" in {
    {
      val sample = "{}"
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation())
      val encoded = serialization.write(decoded)
      encoded should be(sample)
    }
    {
      val sample = """{"id":"12345"}"""
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation(
        id = Some("12345")))
      val encoded = serialization.write(decoded)
      encoded should be(sample)
    }
    {
      val sample = """{"name":"relay name"}"""
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation(
        name = Some("relay name")))
      val encoded = serialization.write(decoded)
      encoded should be(sample)
    }
    {
      val sample = """{"description":"relay desc"}"""
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation(
        description = Some("relay desc")))
      val encoded = serialization.write(decoded)
      encoded should be(sample)
    }
    {
      val sample = """{"pubkey":"62e448d5a67cd9d24511d4024d687473b6f24663e91cd0d83d4b3f77566d0ed8"}"""
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation(
        pubkey = Some(NostrPublicKey.fromHex("62e448d5a67cd9d24511d4024d687473b6f24663e91cd0d83d4b3f77566d0ed8"))))
      val encoded = serialization.write(decoded)
      encoded should be(sample)
    }
    {
      val sample = """{"pubkey":"malformed public key"}"""
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation())
      val encoded = serialization.write(decoded)
      encoded should be("{}")
    }
    {
      val sample = """{"contact":"relay contact"}"""
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation(
        contact = Some("relay contact")))
      val encoded = serialization.write(decoded)
      encoded should be(sample)
    }
    {
      val sample = """{"supported_nips":[1,2,4]}"""
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation(
        supportedNips = Vector(1, 2, 4)))
      val encoded = serialization.write(decoded)
      encoded should be(sample)
    }
    {
      val sample = """{"software":"relay software"}"""
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation(
        software = Some("relay software")
      ))
      val encoded = serialization.write(decoded)
      encoded should be(sample)
    }
    {
      val sample = """{"version":"relay version"}"""
      val decoded = serialization.read[NostrRelayInformation](sample)
      decoded should be(NostrRelayInformation(
        version = Some("relay version")
      ))
      val encoded = serialization.write(decoded)
      encoded should be(sample)
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
    JacksonCodecs.decodeRelayInfo(json1) should be(NostrRelayInformation(
      id = Some("wss://nostr.example.com/"),
      name = Some("nostr-rs-relay"),
      description = Some("A newly created nostr-rs-relay.\n\nCustomize this with your own info."),
      supportedNips = Vector(1,2,9,11,12,15,16,20,22),
      software = Some("https://git.sr.ht/~gheartsfield/nostr-rs-relay"),
      version = Some("0.7.16")
    ))

    val json2 = """{"name":"nostream.your-domain.com","description":"A nostr relay written in TypeScript.","pubkey":"replace-with-your-pubkey","contact":"operator@your-domain.com","supported_nips":[1,2,4,9,11,12,15,16,20,22,26,28,33],"software":"git+https://github.com/Cameri/nostream.git","version":"1.16.0"}"""
    JacksonCodecs.decodeRelayInfo(json2) should be(NostrRelayInformation(
      name = Some("nostream.your-domain.com"),
      description = Some("A nostr relay written in TypeScript."),
      contact = Some("operator@your-domain.com"),
      supportedNips = Vector(1,2,4,9,11,12,15,16,20,22,26,28,33),
      software = Some("git+https://github.com/Cameri/nostream.git"),
      version = Some("1.16.0")
    ))
  }
}
