package snostr.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.core.TestUtil.testVectors

class NostrPrivateKeySpec extends AnyFlatSpec with Matchers {

  it should "process the test vectors" in {
    testVectors().map { case (_, secretKey, publicKey, _, _, _, _, _) =>
      secretKey match {
        case Some(privateKey) =>
          val schnorrPublicKey = privateKey.publicKey
          schnorrPublicKey should be(publicKey)
          1
        case None =>
          0
      }
    }.sum should be(4)
  }

  it should "read/write bech32 representation" in {
    val privateKey = NostrPrivateKey.freshPrivateKey

    val nsec = privateKey.toBech32

    NostrPrivateKey.fromBech32(nsec) should be(privateKey)

    NostrPrivateKey.fromBech32("nsec1vl029mgpspedva04g90vltkh6fvh240zqtv9k0t9af8935ke9laqsnlfe5") should be(NostrPrivateKey.fromHex("67dea2ed018072d675f5415ecfaed7d2597555e202d85b3d65ea4e58d2d92ffa"))
  }

  it should "mask its value in toString" in {
    NostrPrivateKey.freshPrivateKey.toString should be("<private_key>")
    s"${NostrPrivateKey.freshPrivateKey}" should be("<private_key>")
    "(" + NostrPrivateKey.freshPrivateKey + ")" should be("(<private_key>)")
  }
}
