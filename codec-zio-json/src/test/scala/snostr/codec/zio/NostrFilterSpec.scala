package snostr.codec.zio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.JsonDecoders._
import snostr.codec.zio.JsonEncoders._
import snostr.core._
import zio.json.{EncoderOps, JsonDecoder}

class NostrFilterSpec extends AnyFlatSpec with Matchers {
  it should "encode/decode" in {
    val sample1 = """{"ids":["45ea77c05ccb86329b82888e63c3d0cc4d0155fef0ce91e449b8618d13543b50"],"authors":["e1098328a151c09369d15d6844b03fd9208dadf767667b729791f247b4bf5573"],"kinds":[1,2,3],"#e":["ab4f1585f5a8926dde50835877030b6f6186062fd4ce4505b314994da886a599"],"#p":["62e448d5a67cd9d24511d4024d687473b6f24663e91cd0d83d4b3f77566d0ed8"],"since":100,"until":10000,"limit":10}"""
    val etherDecoded1 = JsonDecoder[NostrFilter].decodeJson(sample1)
    etherDecoded1.isRight should be(true)
    val Right(decoded1) = etherDecoded1
    decoded1.ids should be(Vector("45ea77c05ccb86329b82888e63c3d0cc4d0155fef0ce91e449b8618d13543b50"))
    decoded1.authors should be(Vector("e1098328a151c09369d15d6844b03fd9208dadf767667b729791f247b4bf5573"))
    decoded1.kinds should be(Vector(1, 2, 3))
    decoded1.e should be(Vector(Sha256Digest.fromHex("ab4f1585f5a8926dde50835877030b6f6186062fd4ce4505b314994da886a599")))
    decoded1.p should be(Vector(NostrPublicKey.fromHex("62e448d5a67cd9d24511d4024d687473b6f24663e91cd0d83d4b3f77566d0ed8")))
    decoded1.since should be(Some(100))
    decoded1.until should be(Some(10000))
    decoded1.limit should be(Some(10))
    decoded1.toJson should be(sample1)

    val seckey = NostrPrivateKey.freshPrivateKey
    val pubkey = seckey.publicKey

    val filter = NostrFilter(
      ids = Vector(Crypto.sha256(seckey.toByteArray).toHex),
      authors = Vector(pubkey.toHex),
      kinds = Vector(0, 1, 2),
      tags = Map(
        E -> Vector(Crypto.sha256(pubkey.toByteArray).toHex),
        P -> Vector(NostrPrivateKey.freshPrivateKey.publicKey.toHex)),
      since = None,
      until = None,
      limit = None
    )

    val encoded = filter.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrFilter].decodeJson(encoded)

    decoded should be(Right(filter))
  }
}
