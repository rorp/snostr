package snostr.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NostrPublicKeySpec extends AnyFlatSpec with Matchers {

  it should "read/write bech32 representation" in {
    val npub = "npub10elfcs4fr0l0r8af98jlmgdh9c8tcxjvz9qkw038js35mp4dma8qzvjptg"
    val publicKey = NostrPublicKey.fromHex("7e7e9c42a91bfef19fa929e5fda1b72e0ebc1a4c1141673e2794234d86addf4e")

    publicKey.toBech32 should be(npub)

    NostrPublicKey.fromBech32(npub) should be(publicKey)
  }

}
