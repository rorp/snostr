package snostr.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Nip19Spec extends AnyFlatSpec with Matchers {

  it should "read/write note" in {
    val note = Note(Sha256Digest.fromHex("23a9d435681d9c45848ac5ca1edd3975d256906a15bf4d22401a6b79894ec47e"))

    val bech32 = "note1yw5agdtgrkwytpy2ch9pahfewhf9dyr2zkl56gjqrf4hnz2wc3lqj8w45q"

    note.toBech32 should be(bech32)

    val decoded = Note.fromBech32(bech32)

    decoded should be(note)
  }

  it should "read/write naddr" in {

    val naddr = NAddr(
      d = DTag("banana"),
      kind = 37342,
      author = NostrPublicKey.fromHex("e2e973d0aa20a20aae663ce3c2ed8bb161b9b2df96a5e760d399fe675d77461e"),
      relays = Vector("wss://r.x.com", "wss://djbas.sadkb.com")
    )

    val bech32 = "naddr1qqrxyctwv9hxzq3qut5h8592yz3q4tnx8n3u9mvtk9smnvklj6j7wcxnn8lxwhthgc0qxpqqqzgauqgdwaehxw309aezu7pwvdhk6qg4waehxw309ajx5cnpwvh8xctydd3zucm0d5759r6v"

    naddr.toBech32 should be(bech32)

    val decoded = NAddr.fromBech32(bech32)

    decoded should be(naddr)
  }

  it should "read/write nrelay" in {

    val nprofile = NRelay("wss://r.x.com")

    val bech32 = "nrelay1qqxhwumn8ghj7u3w0qhxxmmdzq8v9f"

    nprofile.toBech32 should be(bech32)

    val decoded = NRelay.fromBech32(bech32)

    decoded should be(nprofile)
  }

  it should "read/write nprofile" in {

    val nprofile = NProfile(
      pubkey = NostrPublicKey.fromHex("3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d"),
      relays = Vector("wss://r.x.com", "wss://djbas.sadkb.com")
    )

    val bech32 = "nprofile1qqsrhuxx8l9ex335q7he0f09aej04zpazpl0ne2cgukyawd24mayt8gpp4mhxue69uhhytnc9e3k7mgpz4mhxue69uhkg6nzv9ejuumpv34kytnrdaksjlyr9p"

    nprofile.toBech32 should be(bech32)

    val decoded = NProfile.fromBech32(bech32)

    decoded should be(nprofile)
  }

  it should "read/write nevent" in {
    {
      val nevent = NEvent(
        id = Sha256Digest.fromHex("23a9d435681d9c45848ac5ca1edd3975d256906a15bf4d22401a6b79894ec47e"),
        author = Some(NostrPublicKey.fromHex("2dbe092c6ada1367ecb73a0ef11cd2c31feb32357824c64a18f8da8596214d70")),
        relays = Vector("wss://x.com/"),
        kind = None
      )


      val bech32 = "nevent1qqsz82w5x45pm8z9sj9vtjs7m5uht5jkjp4pt06dyfqp56me398vglspp3mhxue69uhhstnrdakj7q3q9klqjtr2mgfk0m9h8g80z8xjcv07kv340qjvvjsclrdgt93pf4cqur7gsc"

      nevent.toBech32 should be(bech32)

      val decoded = NEvent.fromBech32(bech32)

      decoded should be(nevent)
    }
    {
          val nevent = NEvent(
            id = Sha256Digest.fromHex(List.fill(32)("00").mkString),
            author = Some(NostrPublicKey.fromHex("3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d")),
            relays = Vector("wss://r.x.com", "wss://djbas.sadkb.com"),
            kind = Some(0x01020304)
          )

          val bech32 = "nevent1qqsqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqpp4mhxue69uhhytnc9e3k7mgpz4mhxue69uhkg6nzv9ejuumpv34kytnrdaksygpm7rrrljungc6q0tuh5hj7ue863q73qlheu4vywtzwhx42a7j9n5psgqgzqvzqy9f5lm"

          nevent.toBech32 should be(bech32)

          val decoded = NEvent.fromBech32(bech32)

          decoded should be(nevent)
    }
  }
}