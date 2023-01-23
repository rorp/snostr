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
  }

  it should "mask its value in toString" in {
    NostrPrivateKey.freshPrivateKey.toString should be("<private_key>")
    s"${NostrPrivateKey.freshPrivateKey}" should be("<private_key>")
    "(" + NostrPrivateKey.freshPrivateKey + ")" should be("(<private_key>)")
  }
}
