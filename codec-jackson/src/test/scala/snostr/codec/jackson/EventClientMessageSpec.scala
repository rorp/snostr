package snostr.codec.jackson

import com.fasterxml.jackson.databind.exc.MismatchedInputException
import org.json4s.MappingException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.jackson.JsonSerializers.{formats, serialization}
import snostr.core._

class EventClientMessageSpec extends AnyFlatSpec with Matchers {
  it should "serialize/deserialize" in {
    assertThrows[MismatchedInputException](serialization.read[EventClientMessage](""))
    assertThrows[MappingException](serialization.read[EventClientMessage]("[]"))
    assertThrows[MappingException](serialization.read[EventClientMessage]("""["EVENT"]"""))
    assertThrows[MappingException](serialization.read[EventClientMessage]("""["eVENt",{"id":"07903a662720a33dc334544687ca4d460d7ea5b3d28e978ecc06ecaf4c17747e","pubkey":"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at":1671663042,"kind":1,"tags":[["e","a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"]],"content":"αυτό είναι ένα μήνυμα","sig":"a3b66bfad7ee596bc01aa85cb797493982e43525fb70d3b30b46c3814b72213ff3efcdb2e38a12d0cba18ae4e7b8288f446233fa95bd5086a6b1a1bb7f5bb26a"}]"""))
    assertThrows[MappingException](serialization.read[EventClientMessage]("""["EVENT",{"id":"07903a662720a33dc334544687ca4d460d7ea5b3d28e978ecc06ecaf4c17747e","pubkey":"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at":1671663042,"kind":1,"tags":[["e","a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"]],"content":"αυτό είναι ένα μήνυμα","sig":"a3b66bfad7ee596bc01aa85cb797493982e43525fb70d3b30b46c3814b72213ff3efcdb2e38a12d0cba18ae4e7b8288f446233fa95bd5086a6b1a1bb7f5bb26a"},"extra"]"""))

    val json = """["EVENT",{"id":"07903a662720a33dc334544687ca4d460d7ea5b3d28e978ecc06ecaf4c17747e","pubkey":"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at":1671663042,"kind":1,"tags":[["e","a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"]],"content":"αυτό είναι ένα μήνυμα","sig":"a3b66bfad7ee596bc01aa85cb797493982e43525fb70d3b30b46c3814b72213ff3efcdb2e38a12d0cba18ae4e7b8288f446233fa95bd5086a6b1a1bb7f5bb26a"}]"""

    val decoded = serialization.read[EventClientMessage](json)

    decoded.event.id should be(Sha256Digest.fromHex("07903a662720a33dc334544687ca4d460d7ea5b3d28e978ecc06ecaf4c17747e"))
    decoded.event.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    decoded.event.createdAt.getEpochSecond should be(1671663042)
    decoded.event.kind.content should be("αυτό είναι ένα μήνυμα")
    decoded.event.kind.tags should be(Vector(
      ETag(Sha256Digest.fromHex("a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"), None, None),
      PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"), None, None)))
    decoded.event.sig should be(NostrSignature.fromHex("a3b66bfad7ee596bc01aa85cb797493982e43525fb70d3b30b46c3814b72213ff3efcdb2e38a12d0cba18ae4e7b8288f446233fa95bd5086a6b1a1bb7f5bb26a"))

    val encoded = serialization.write(decoded)

    encoded should be(json)

    JacksonCodecs.decodeClientMessage(encoded) should be(decoded)
    JacksonCodecs.encodeClientMessage(decoded) should be(json)
  }
}
