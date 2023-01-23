package snostr.core

import fr.acinq.bitcoin.ByteVector32
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Sha256DigestSpec extends AnyFlatSpec with Matchers {
  it should "be able to hash" in {
    Crypto.sha256(ByteVector32.Zeroes) shouldBe Sha256Digest.fromHex("66687aadf862bd776c8fc18b8e9f8e20089714856ee233b3902a591d0d5f2925")
  }
}
