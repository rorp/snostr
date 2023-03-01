package snostr.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class NostrEventSpec extends AnyFlatSpec with Matchers {

  it should "decrypt NIP04 content" in {
    val pubkey = NostrPublicKey.fromHex("c4e410f6be7387d571798a74c7cd3708dea887782469fdab00615e084cf590e9")

    val kind = EncryptedDirectMessage(
      content = "euGNL5bCtvYnPYqkaTMLeEw17CjGaAtboJkIFGJfwt9vql/9czxztEFf+c90aQbM?iv=o6k6z6LX+VPHnKXWgbnccA==",
      receiverPublicKey = NostrPublicKey.fromHex("21634395da38d7940ddb4fe31504e9529395ee09662f97ae3937fd9e3e53148a"),
      senderPublicKey = pubkey
    )

    val event = NostrEvent(
      id = Sha256Digest.fromHex("38071f282f763c18c4dd58c50b91ad6062df3d0e2836093f404b9a1481efe271"),
      pubkey = pubkey,
      createdAt = Instant.ofEpochSecond(1673828899),
      kind = kind,
      sig = NostrSignature.fromHex("b818a61c2bcfbc88df8ffd21eb3ec608c4ac704e932c6fc31c6b1e626dd190f7afe6de0e78c14a0d1ec5293f88a947da2e62eb49cc2e563e9e9ea307117cf6cd")
    )

    val receiverPrivateKey = NostrPrivateKey.fromHex("e70390ea48a732cf5d23e43a57e9a29e9da5911d2471be7fe469307e2e4be565")

    event.validSignature should be(true)
    val decrypted1 = kind.decryptForReceiver(receiverPrivateKey)
    decrypted1 should be("Αυτό είναι ένα μήνυμα")

    val receiverPublicKey = kind.tags.collectFirst {
      case p: PTag => p.pubkey
    }
    receiverPublicKey shouldNot be(empty)
    receiverPublicKey.get should be(receiverPrivateKey.publicKey)
  }
}
