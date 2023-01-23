package snostr.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NostrPublicKeySpec extends AnyFlatSpec with Matchers {

  it should "read/write bech32 representation" in {
    val npub = "npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
    val publicKey = NostrPublicKey.fromHex("3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d")

    publicKey.toBech32 should be(npub)

    NostrPublicKey.fromBech32(npub) should be(publicKey)
  }

}
