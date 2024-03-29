package snostr.codec.zio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.codec.zio.JsonDecoders._
import snostr.codec.zio.JsonEncoders._
import snostr.core.ETag.{Mention, Reply}
import snostr.core._
import zio.json.{EncoderOps, JsonDecoder}

import java.time.Instant

class NostrEventSpec extends AnyFlatSpec with Matchers {

  implicit val codecs = ZioJsonCodecs

  it should "create set metadata messages" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val pubkey = seckey.publicKey

    val event = NostrEvent.setMetadata(
      privateKey = seckey,
      name = Some("bob"),
      about = Some(""" {"test" : "abc"} " \ / """),
      picture = Some("photograph"),
      nip05 = Some(Nip05Identifier("local-part", "domain-part")),
      createdAt = Instant.ofEpochSecond(1671663042L),
      tags = Vector(
        ETag(Crypto.sha256(pubkey.toByteArray), None, None),
        PTag(pubkey, None, None),
      )
    )

    event.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    event.id should be(Sha256Digest.fromHex("0af3422180d04c304add80c83f9f90d175fd50bea2aba08012549691b3d2b307"))
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[SetMetadata])
    val kind = event.kind.asInstanceOf[SetMetadata]
    kind.value should be(0)
    kind.about should be(Some(""" {"test" : "abc"} " \ / """))
    kind.name should be(Some("bob"))
    kind.nip05 should be(Some(Nip05Identifier("local-part", "domain-part")))
    kind.picture should be(Some("photograph"))
    kind.content should be("""{"about":" {\"test\" : \"abc\"} \" \\ / ","name":"bob","nip05":"local-part@domain-part","picture":"photograph"}""")
    kind.tags should be(Vector(
      ETag(Crypto.sha256(pubkey.toByteArray), None, None),
      PTag(pubkey, None, None),
    ))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(a[SetMetadata]))
    decoded.foreach(_.commitment should be(event.commitment))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_.kind.tags should be(Vector(
      ETag(Crypto.sha256(pubkey.toByteArray), None, None),
      PTag(pubkey, None, None),
    )))
  }

  it should "create text notes" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val pubkey = seckey.publicKey

    val event = NostrEvent.textNote(
      privateKey = seckey,
      createdAt = Instant.ofEpochSecond(1671663042L),
      expiration = Some(Instant.ofEpochSecond(1671664000L)),
      tags = Vector(
        NonceTag(12345, 10),
        ETag(Crypto.sha256(pubkey.toByteArray), None, None),
        PTag(pubkey, None, None),
      ),
      subject = Some("this is a subject"),
      content = "Αυτό είναι ένα μήνυμα")

    event.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    event.id should be(Sha256Digest.fromHex("de7918340da7f13ba3ec20b1176f5e4c9f2c11480fb288e0ae5ce504178bba59"))
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[TextNote])
    val kind = event.kind.asInstanceOf[TextNote]
    kind.value should be(1)
    kind.content should be("Αυτό είναι ένα μήνυμα")
    kind.tags should be(Vector(
      SubjectTag("this is a subject"),
      NonceTag(12345, 10),
      ETag(Crypto.sha256(pubkey.toByteArray), None, None),
      PTag(pubkey, None, None),
      ExpirationTag(Instant.ofEpochSecond(1671664000L))
    ))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(a[TextNote]))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_ should be(event))
    decoded.foreach(_.kind.tags should be(Vector(
      SubjectTag("this is a subject"),
      NonceTag(12345, 10),
      ETag(Crypto.sha256(pubkey.toByteArray), None, None),
      PTag(pubkey, None, None),
      ExpirationTag(Instant.ofEpochSecond(1671664000L))
    )))
  }

  it should "create recommend server messages" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val pubkey = seckey.publicKey

    val event = NostrEvent.recommendServer(
      privateKey = seckey,
      createdAt = Instant.ofEpochSecond(1671663042L),
      url = "relay")

    event.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    event.id should be(Sha256Digest.fromHex("1c9fc5064286cc15399e6edf0f4ad74f18b008c010b9085231e1c21cfb456c8e"))
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[RecommendServer])
    val kind = event.kind.asInstanceOf[RecommendServer]
    kind.value should be(2)
    kind.url should be("relay")
    kind.content should be("relay")
    kind.tags should be(empty)

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(a[RecommendServer]))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_ should be(event))
    decoded.foreach(_.kind.tags should be(empty))
  }

  it should "create contact lists" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val pubkey = seckey.publicKey

    val contacts = Vector(
      ContactList.Contact(publicKey = pubkey, mainRelayUrl = "main relay", petname = "fido")
    )

    val event = NostrEvent.contactList(
      privateKey = seckey,
      createdAt = Instant.ofEpochSecond(1671663042L),
      contacts = contacts)

    event.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    event.id should be(Sha256Digest.fromHex("034a62112055611858ab9137e8b57f5ba6bbd30aaf2e17066ef82778866e877e"))
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[ContactList])
    val kind = event.kind.asInstanceOf[ContactList]
    kind.value should be(3)
    kind.contacts should be(contacts)
    kind.content should be("")
    kind.tags should be(Vector(PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"), Some("main relay"), Some("fido"))))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(a[ContactList]))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_.commitment should be(event.commitment))
    decoded.foreach(_.kind.tags should be(Vector(PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"), Some("main relay"), Some("fido")))))
  }

  it should "create NIP-04 encrypted direct messages" in {
    val ourSeckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")
    val ourPubkey = ourSeckey.publicKey

    val theirSeckey = NostrPrivateKey.freshPrivateKey
    val theirPubkey = theirSeckey.publicKey

    val event = NostrEvent.encryptedDirectMessage(
      senderPrivateKey = ourSeckey,
      receiverPublicKey = theirPubkey,
      createdAt = Instant.ofEpochSecond(1671663042L),
      content = "Αυτό είναι ένα μήνυμα",
      nipNumber = 4
    )

    event.pubkey should be(ourPubkey)
    event.createdAt.getEpochSecond should be(1671663042L)
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[EncryptedDirectMessage])
    event.kind should be(a[EncryptedDirectMessage04])
    val kind = event.kind.asInstanceOf[EncryptedDirectMessage04]
    kind.senderPublicKey should be(ourPubkey)
    kind.receiverPublicKey should be(theirPubkey)
    kind.value should be(4)
    kind.content.contains("?iv=") should be(true)
    kind.decryptForReceiver(theirSeckey) should be("Αυτό είναι ένα μήνυμα")
    kind.decryptForSender(ourSeckey) should be("Αυτό είναι ένα μήνυμα")
    kind.tags should be(Vector(PTag(theirPubkey, None, None)))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(an[EncryptedDirectMessage04]))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_.commitment should be(event.commitment))
    decoded.foreach(_.kind.tags should be(Vector(PTag(theirPubkey, None, None))))
  }

  it should "create NIP-44 encrypted direct messages" in {
    val ourSeckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")
    val ourPubkey = ourSeckey.publicKey

    val theirSeckey = NostrPrivateKey.freshPrivateKey
    val theirPubkey = theirSeckey.publicKey

    val eventId = Sha256Digest.fromHex("0000000000000000000000000000000000000000000000000000000000000001")

    val event = NostrEvent.encryptedDirectMessage(
      senderPrivateKey = ourSeckey,
      receiverPublicKey = theirPubkey,
      createdAt = Instant.ofEpochSecond(1671663042L),
      tags = Vector(ETag(eventId, None, None)),
      content = "Αυτό είναι ένα μήνυμα"
    )

    event.pubkey should be(ourPubkey)
    event.createdAt.getEpochSecond should be(1671663042L)
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[EncryptedDirectMessage])
    event.kind should be(a[EncryptedDirectMessage44])
    val kind = event.kind.asInstanceOf[EncryptedDirectMessage44]
    kind.senderPublicKey should be(ourPubkey)
    kind.receiverPublicKey should be(theirPubkey)
    kind.value should be(44)
    kind.content.startsWith("1,") should be(true)
    kind.content.count(_ == ',') should be(2)
    kind.decryptForReceiver(theirSeckey) should be("Αυτό είναι ένα μήνυμα")
    kind.decryptForSender(ourSeckey) should be("Αυτό είναι ένα μήνυμα")
    kind.tags should be(Vector(
      PTag(theirPubkey, None, None),
      ETag(eventId, None, None)))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(an[EncryptedDirectMessage44]))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_.commitment should be(event.commitment))
    decoded.foreach(_.kind.tags should be(Vector(
      PTag(theirPubkey, None, None),
      ETag(eventId, None, None))))
  }

  it should "create deletion messages" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val ids = Vector(
      Sha256Digest.fromHex("1c9fc5064286cc15399e6edf0f4ad74f18b008c010b9085231e1c21cfb456c8e"),
      Sha256Digest.fromHex("034a62112055611858ab9137e8b57f5ba6bbd30aaf2e17066ef82778866e877e"))

    val event = NostrEvent.deletion(
      privateKey = seckey,
      eventIds = ids,
      content = "these posts were published by accident",
      createdAt = Instant.ofEpochSecond(1671663042L))

    event.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    event.id should be(Sha256Digest.fromHex("cde40c6eff37cb9c4565b420ff0ce78acb381b6a406b4119228e5ba4a1d26601"))
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[Deletion])
    val kind = event.kind.asInstanceOf[Deletion]
    kind.value should be(5)
    kind.content should be("these posts were published by accident")
    kind.tags should be(Vector(
      ETag(Sha256Digest.fromHex("1c9fc5064286cc15399e6edf0f4ad74f18b008c010b9085231e1c21cfb456c8e"), None, None),
      ETag(Sha256Digest.fromHex("034a62112055611858ab9137e8b57f5ba6bbd30aaf2e17066ef82778866e877e"), None, None)))
    kind.eventIds should be(ids)

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(a[Deletion]))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_.commitment should be(event.commitment))
    decoded.foreach(_.kind.tags should be(Vector(
      ETag(Sha256Digest.fromHex("1c9fc5064286cc15399e6edf0f4ad74f18b008c010b9085231e1c21cfb456c8e"), None, None),
      ETag(Sha256Digest.fromHex("034a62112055611858ab9137e8b57f5ba6bbd30aaf2e17066ef82778866e877e"), None, None))))
  }

  it should "create repost messages" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val reposted = NostrEvent.textNote(
      privateKey = NostrPrivateKey.fromHex(seckey.toHex.reverse),
      createdAt = Instant.ofEpochSecond(1671663000L),
      content = "Αυτό είναι ένα μήνυμα")

    reposted.id should be(Sha256Digest.fromHex("d35ce9ba45c211ba6807ecc7550b1ef4d2ed08bdff343a4fe1e01e2fa5f79e8e"))
    reposted.pubkey should be(NostrPublicKey.fromHex("42bf015edf959fa28f986c9af9aee2811d9d1f0395188836a2c2162e51662e7c"))

    val event = NostrEvent.repost(
      privateKey = seckey,
      event = reposted,
      relay = "ws://relay",
      createdAt = Instant.ofEpochSecond(1671663042L))

    event.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    event.id should be(Sha256Digest.fromHex("113738b41325a7d470995bf9e8ab72ada4f5b837998a4ffb5f89e06d0afc9612"))
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[Repost])
    val kind = event.kind.asInstanceOf[Repost]
    kind.value should be(6)
    kind.content should be("")
    kind.tags should be(
      Vector(
        ETag(Sha256Digest.fromHex("d35ce9ba45c211ba6807ecc7550b1ef4d2ed08bdff343a4fe1e01e2fa5f79e8e"), Some("ws://relay"), Some(Mention)),
        PTag(NostrPublicKey.fromHex("42bf015edf959fa28f986c9af9aee2811d9d1f0395188836a2c2162e51662e7c"), None, None)
      ))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(a[Repost]))
    decoded.foreach(_.commitment should be(event.commitment))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_.kind.tags should be(
      Vector(
        ETag(Sha256Digest.fromHex("d35ce9ba45c211ba6807ecc7550b1ef4d2ed08bdff343a4fe1e01e2fa5f79e8e"), Some("ws://relay"), Some(Mention)),
        PTag(NostrPublicKey.fromHex("42bf015edf959fa28f986c9af9aee2811d9d1f0395188836a2c2162e51662e7c"), None, None)
      )))
  }

  it should "create reaction messages" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val reactTo = NostrEvent.textNote(
      privateKey = NostrPrivateKey.fromHex(seckey.toHex.reverse),
      createdAt = Instant.ofEpochSecond(1671663000L),
      tags = Vector(
        NonceTag(12345, 10),
        ETag(Sha256Digest.fromHex("b496eae2905887c20f47507c26b68fd21f55dcd1aeee3188bb36a79ae15255de"), Some("ws://relay"), None),
        PTag(NostrPublicKey.fromHex("d7e71c0cf8c527ae5615e503fd4079399e49479fc8d22d21a561a74ea3f279b5"), None, None),
      ),
      content = "Αυτό είναι ένα μήνυμα")

    reactTo.id should be(Sha256Digest.fromHex("547902bf83e37ee02c596c24ffd3865ffc903e80763a2b75f357d316e2502ac7"))
    reactTo.pubkey should be(NostrPublicKey.fromHex("42bf015edf959fa28f986c9af9aee2811d9d1f0395188836a2c2162e51662e7c"))
    reactTo.kind.tags should be(
      Vector(
        NonceTag(12345, 10),
        ETag(Sha256Digest.fromHex("b496eae2905887c20f47507c26b68fd21f55dcd1aeee3188bb36a79ae15255de"), Some("ws://relay"), None),
        PTag(NostrPublicKey.fromHex("d7e71c0cf8c527ae5615e503fd4079399e49479fc8d22d21a561a74ea3f279b5"), None, None),
      ))

    val event = NostrEvent.reaction(
      privateKey = seckey,
      event = reactTo,
      content = "+",
      createdAt = Instant.ofEpochSecond(1671663042L))

    event.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    event.id should be(Sha256Digest.fromHex("f8e68e132106c0cd7a9678965f03dc371f79e67a5b56039ce79b08986710f5ff"))
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[Reaction])
    val kind = event.kind.asInstanceOf[Reaction]
    kind.value should be(7)
    kind.content should be("+")
    kind.tags should be(reactTo.kind.tags.drop(1) ++
      Vector(
        ETag(Sha256Digest.fromHex("547902bf83e37ee02c596c24ffd3865ffc903e80763a2b75f357d316e2502ac7"), None, None),
        PTag(NostrPublicKey.fromHex("42bf015edf959fa28f986c9af9aee2811d9d1f0395188836a2c2162e51662e7c"), None, None),
      ))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(a[Reaction]))
    decoded.foreach(_.commitment should be(event.commitment))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_.kind.tags should be(reactTo.kind.tags.drop(1) ++
      Vector(
        ETag(Sha256Digest.fromHex("547902bf83e37ee02c596c24ffd3865ffc903e80763a2b75f357d316e2502ac7"), None, None),
        PTag(NostrPublicKey.fromHex("42bf015edf959fa28f986c9af9aee2811d9d1f0395188836a2c2162e51662e7c"), None, None),
      )))
  }

  it should "create gift wrap messages" in {
    val ourSeckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")
    val ourPubkey = ourSeckey.publicKey

    val theirSeckey = NostrPrivateKey.freshPrivateKey
    val theirPubkey = theirSeckey.publicKey

    val wrappedEvent = NostrEvent.textNote(
      privateKey = NostrPrivateKey.freshPrivateKey,
      createdAt = Instant.ofEpochSecond(1671663042L),
      content = "Αυτό είναι ένα μήνυμα")

    val event = NostrEvent.giftWrap(
      senderPrivateKey = ourSeckey,
      receiverPublicKey = theirPubkey,
      createdAt = Instant.ofEpochSecond(1671663042L),
      event = wrappedEvent
    )

    event.pubkey should be(ourPubkey)
    event.createdAt.getEpochSecond should be(1671663042L)
    event.validId should be(true)
    event.validSignature should be(true)
    event.kind should be(a[GiftWrap])
    val kind = event.kind.asInstanceOf[GiftWrap]
    kind.senderPublicKey should be(ourPubkey)
    kind.receiverPublicKey should be(theirPubkey)
    kind.value should be(1059)
    kind.content.contains(""""version":1""") should be(true)
    kind.content.count(_ == ',') should be(2)
    kind.decryptForReceiver(theirSeckey) should be(wrappedEvent)
    kind.decryptForSender(ourSeckey) should be(wrappedEvent)
    kind.tags should be(Vector(PTag(theirPubkey, None, None)))
    kind.parsedContent should be(None)
    kind.parsedTags should be(Vector.empty)

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(an[GiftWrap]))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_.commitment should be(event.commitment))
    decoded.foreach(_.kind.tags should be(Vector(PTag(theirPubkey, None, None))))
    decoded.foreach(_.kind.parsedContent should not be None)
    decoded.foreach(_.kind.parsedTags should be(event.tags))
    decoded.foreach(_.kind.asInstanceOf[GiftWrap].decryptForReceiver(theirSeckey) should be(wrappedEvent))
    decoded.foreach(_.kind.asInstanceOf[GiftWrap].decryptForSender(ourSeckey) should be(wrappedEvent))
  }

  it should "create auth messages" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val event = NostrEvent.authMessage(
      privateKey = NostrPrivateKey.fromHex(seckey.toHex.reverse),
      challenge = "auth challenge",
      relay = "wss://test.relay/",
      createdAt = Instant.ofEpochSecond(1671663000L))

    event.id should be(Sha256Digest.fromHex("00996da3e35350c631e5c866834b5b1457c990a3727bf4ed1c3be7d951b181c0"))
    event.pubkey should be(NostrPublicKey.fromHex("42bf015edf959fa28f986c9af9aee2811d9d1f0395188836a2c2162e51662e7c"))
    event.createdAt.getEpochSecond should be(1671663000L)
    event.kind.tags should be(
      Vector(
        ChallengeTag("auth challenge"),
        RelayTag("wss://test.relay/"),
      ))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)
    decoded.foreach(_.kind should be(an[Auth]))
    decoded.foreach(_.commitment should be(event.commitment))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
    decoded.foreach(_.tags.collect { case t: ChallengeTag => t } shouldNot be(empty))
    decoded.foreach(_.tags.collect { case t: RelayTag => t } shouldNot be(empty))
    decoded.foreach(_.kind.tags should be(
      Vector(
        ChallengeTag("auth challenge"),
        RelayTag("wss://test.relay/"),
      )))
  }

  it should "create zap requests" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val recipient = NostrPublicKey.fromHex("58a326b4314e934f90f305aec17b47fd8d69bcfc38ee428b2ef25db0938f757c")

    val event = NostrEvent.zapRequest(
      privateKey = NostrPrivateKey.fromHex(seckey.toHex.reverse),
      relays = Vector("wss://nostr-pub.wellorder.com", "wss://anotherrelay.example.com"),
      amount = 1234567890,
      lnurl = "lnurl1dp68gurn8ghj7um5v93kketj9ehx2amn9uh8wetvdskkkmn0wahz7mrww4excup0dajx2mrv92x9xp",
      recipient = recipient,
      eventId = Some(Sha256Digest.fromHex("1a1badfa0b574c236577777daa4ae9fbc76805d47f26db8c8c47d61363e6504c")),
      aTag = Some(ATag(1, recipient)),
      createdAt = Instant.ofEpochSecond(1671663000L))

    event.id should be(Sha256Digest.fromHex("48fdd89f52aa657cfb96f70c75aa4f53227f088372341e520d706ad4011b4799"))
    event.pubkey should be(NostrPublicKey.fromHex("42bf015edf959fa28f986c9af9aee2811d9d1f0395188836a2c2162e51662e7c"))
    event.createdAt.getEpochSecond should be(1671663000L)
    event.kind.tags should be(
      Vector(
        RelaysTag(Vector("wss://nostr-pub.wellorder.com", "wss://anotherrelay.example.com")),
        AmountTag(1234567890),
        LNURLTag("lnurl1dp68gurn8ghj7um5v93kketj9ehx2amn9uh8wetvdskkkmn0wahz7mrww4excup0dajx2mrv92x9xp"),
        PTag(NostrPublicKey.fromHex("58a326b4314e934f90f305aec17b47fd8d69bcfc38ee428b2ef25db0938f757c")),
        ETag(Sha256Digest.fromHex("1a1badfa0b574c236577777daa4ae9fbc76805d47f26db8c8c47d61363e6504c")),
        ATag(1, recipient)
      ))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded).getOrElse(throw new Exception())
    decoded.kind should be(a[ZapRequest])
    decoded.pubkey should be(event.pubkey)
    decoded.commitment should be(event.commitment)
    decoded.validId should be(true)
    decoded.validSignature should be(true)
    decoded.kind.tags should be(
      Vector(
        RelaysTag(Vector("wss://nostr-pub.wellorder.com", "wss://anotherrelay.example.com")),
        AmountTag(1234567890),
        LNURLTag("lnurl1dp68gurn8ghj7um5v93kketj9ehx2amn9uh8wetvdskkkmn0wahz7mrww4excup0dajx2mrv92x9xp"),
        PTag(NostrPublicKey.fromHex("58a326b4314e934f90f305aec17b47fd8d69bcfc38ee428b2ef25db0938f757c")),
        ETag(Sha256Digest.fromHex("1a1badfa0b574c236577777daa4ae9fbc76805d47f26db8c8c47d61363e6504c")),
        ATag(1, recipient)
      ))

    val decodedKind = decoded.kind.asInstanceOf[ZapRequest]
    decodedKind.relays should be(Vector("wss://nostr-pub.wellorder.com", "wss://anotherrelay.example.com"))
    decodedKind.amount should be(1234567890L)
    decodedKind.lnurl should be("lnurl1dp68gurn8ghj7um5v93kketj9ehx2amn9uh8wetvdskkkmn0wahz7mrww4excup0dajx2mrv92x9xp")
    decodedKind.recipient should be(NostrPublicKey.fromHex("58a326b4314e934f90f305aec17b47fd8d69bcfc38ee428b2ef25db0938f757c"))
    decodedKind.eventId should be(Some(Sha256Digest.fromHex("1a1badfa0b574c236577777daa4ae9fbc76805d47f26db8c8c47d61363e6504c")))
    decodedKind.aTag should be(Some(ATag(1, NostrPublicKey.fromHex("58a326b4314e934f90f305aec17b47fd8d69bcfc38ee428b2ef25db0938f757c"))))
  }

  it should "create zap receipts" in {
    val seckey = NostrPrivateKey.fromHex("03122784000d0403740ecbb75d6e36217cc85b9c438ae62e4379ffea77d4ec8e")

    val recipient = NostrPublicKey.fromHex("7bfdefb934fb19e62f136942afed7df7989cc910ecf2d4ee19c8de090ea77278")

    val sender = NostrPublicKey.fromHex("58a326b4314e934f90f305aec17b47fd8d69bcfc38ee428b2ef25db0938f757c")

    val event = NostrEvent.zapReceipt(
      privateKey = NostrPrivateKey.fromHex(seckey.toHex.reverse),
      recipient,
      bolt11 = "bolt11",
      description = "description",
      preimage = Some("preimage"),
      eventId = Some(Sha256Digest.fromHex("1a1badfa0b574c236577777daa4ae9fbc76805d47f26db8c8c47d61363e6504c")),
      aTag = Some(ATag(1, seckey.publicKey)),
      sender = Some(sender),
      createdAt = Instant.ofEpochSecond(1671663000L))

    event.id should be(Sha256Digest.fromHex("4058129ca5c19fb84999be4ee42adc34cf1b6b73a5fd602b1409f8e483eff45a"))
    event.pubkey should be(NostrPublicKey.fromHex("42bf015edf959fa28f986c9af9aee2811d9d1f0395188836a2c2162e51662e7c"))
    event.createdAt.getEpochSecond should be(1671663000L)
    event.kind.tags should be(
      Vector(
        PTag(NostrPublicKey.fromHex("7bfdefb934fb19e62f136942afed7df7989cc910ecf2d4ee19c8de090ea77278")),
        BigPTag(NostrPublicKey.fromHex("58a326b4314e934f90f305aec17b47fd8d69bcfc38ee428b2ef25db0938f757c")),
        ETag(Sha256Digest.fromHex("1a1badfa0b574c236577777daa4ae9fbc76805d47f26db8c8c47d61363e6504c")),
        ATag(1, seckey.publicKey),
        Bolt11Tag("bolt11"),
        DescriptionTag("description"),
        PreimageTag("preimage"),
      ))

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded).getOrElse(throw new Exception())
    decoded.kind should be(a[ZapReceipt])
    decoded.pubkey should be(event.pubkey)
    decoded.commitment should be(event.commitment)
    decoded.validId should be(true)
    decoded.validSignature should be(true)
    decoded.kind.tags should be(
      Vector(
        PTag(NostrPublicKey.fromHex("7bfdefb934fb19e62f136942afed7df7989cc910ecf2d4ee19c8de090ea77278")),
        BigPTag(NostrPublicKey.fromHex("58a326b4314e934f90f305aec17b47fd8d69bcfc38ee428b2ef25db0938f757c")),
        ETag(Sha256Digest.fromHex("1a1badfa0b574c236577777daa4ae9fbc76805d47f26db8c8c47d61363e6504c")),
        ATag(1, seckey.publicKey),
        Bolt11Tag("bolt11"),
        DescriptionTag("description"),
        PreimageTag("preimage"),
      ))

    val decodedKind = decoded.kind.asInstanceOf[ZapReceipt]
    decodedKind.recipient should be(NostrPublicKey.fromHex("7bfdefb934fb19e62f136942afed7df7989cc910ecf2d4ee19c8de090ea77278"))
    decodedKind.bolt11 should be("bolt11")
    decodedKind.description should be("description")
    decodedKind.preimage should be(Some("preimage"))
    decodedKind.eventId should be(Some(Sha256Digest.fromHex("1a1badfa0b574c236577777daa4ae9fbc76805d47f26db8c8c47d61363e6504c")))
    decodedKind.aTag should be(Some(ATag(1, NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))))
    decodedKind.sender should be(Some(NostrPublicKey.fromHex("58a326b4314e934f90f305aec17b47fd8d69bcfc38ee428b2ef25db0938f757c")))
  }

  it should "encode/decode" in {
    // valid message
    val sample1 = """{"id":"77eea5982de440812d0de1656dced284671d1144ceca1764ae22cfe4745ac3af","pubkey":"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at":1671663042,"kind":1,"tags":[["e","a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"]],"content":"P0ejdbB+NAzYChIE9GX6obuRVL8W70dE/zurduic0L6Qv57zPN1aC3dcHKeYAN1O?iv=L5hVzDp+2S4e/aky9fV+aQ==","sig":"0b59d24567adc5c841c77b8d9c626531e2578aaf9fcbd754bf694d10217677396144a8d1792b09519856a2f82d20449d6d0dcdf8f5266055ec2f3d3385daadd4"}"""
    val eitherDecoded1 = JsonDecoder[NostrEvent].decodeJson(sample1)
    eitherDecoded1.isRight should be(true)
    val Right(decoded1) = eitherDecoded1
    decoded1.id should be(Sha256Digest.fromHex("77eea5982de440812d0de1656dced284671d1144ceca1764ae22cfe4745ac3af"))
    decoded1.validId should be(true)
    decoded1.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    decoded1.createdAt.getEpochSecond should be(1671663042)
    decoded1.kind.value should be(1)
    decoded1.kind.tags.size should be(3)
    decoded1.kind.tags(0) should be(ETag(Sha256Digest.fromHex("a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"), None, None))
    decoded1.kind.tags(1) should be(PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"), None, None))
    decoded1.kind.tags(2) should be(PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"), None, None))
    decoded1.kind.content should be("P0ejdbB+NAzYChIE9GX6obuRVL8W70dE/zurduic0L6Qv57zPN1aC3dcHKeYAN1O?iv=L5hVzDp+2S4e/aky9fV+aQ==")
    decoded1.sig should be(NostrSignature.fromHex("0b59d24567adc5c841c77b8d9c626531e2578aaf9fcbd754bf694d10217677396144a8d1792b09519856a2f82d20449d6d0dcdf8f5266055ec2f3d3385daadd4"))
    decoded1.validSignature should be(true)
    decoded1.kind should be(a[TextNote])
    decoded1.toJson should be(sample1)

    // invalid id
    val sample2 = """{"id":"00eea5982de440812d0de1656dced284671d1144ceca1764ae22cfe4745ac3af","pubkey":"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at":1671663042,"kind":1,"tags":[["e","a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"]],"content":"P0ejdbB+NAzYChIE9GX6obuRVL8W70dE/zurduic0L6Qv57zPN1aC3dcHKeYAN1O?iv=L5hVzDp+2S4e/aky9fV+aQ==","sig":"0b59d24567adc5c841c77b8d9c626531e2578aaf9fcbd754bf694d10217677396144a8d1792b09519856a2f82d20449d6d0dcdf8f5266055ec2f3d3385daadd4"}"""
    val eitherDecoded2 = JsonDecoder[NostrEvent].decodeJson(sample2)
    eitherDecoded2.isRight should be(true)
    val Right(decoded2) = eitherDecoded2
    decoded2.id should be(Sha256Digest.fromHex("00eea5982de440812d0de1656dced284671d1144ceca1764ae22cfe4745ac3af"))
    decoded2.validId should be(false)
    decoded2.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    decoded2.createdAt.getEpochSecond should be(1671663042)
    decoded2.kind.value should be(1)
    decoded2.kind.tags.size should be(3)
    decoded2.kind.tags(0) should be(ETag(Sha256Digest.fromHex("a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"), None, None))
    decoded2.kind.tags(1) should be(PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"), None, None))
    decoded2.kind.tags(2) should be(PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"), None, None))
    decoded2.kind.content should be("P0ejdbB+NAzYChIE9GX6obuRVL8W70dE/zurduic0L6Qv57zPN1aC3dcHKeYAN1O?iv=L5hVzDp+2S4e/aky9fV+aQ==")
    decoded2.sig should be(NostrSignature.fromHex("0b59d24567adc5c841c77b8d9c626531e2578aaf9fcbd754bf694d10217677396144a8d1792b09519856a2f82d20449d6d0dcdf8f5266055ec2f3d3385daadd4"))
    decoded2.validSignature should be(false)
    decoded2.kind should be(a[TextNote])
    decoded2.toJson should be(sample2)

    // invalid signature
    val sample3 = """{"id":"77eea5982de440812d0de1656dced284671d1144ceca1764ae22cfe4745ac3af","pubkey":"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at":1671663042,"kind":1,"tags":[["e","a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"],["p","a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"]],"content":"P0ejdbB+NAzYChIE9GX6obuRVL8W70dE/zurduic0L6Qv57zPN1aC3dcHKeYAN1O?iv=L5hVzDp+2S4e/aky9fV+aQ==","sig":"ff59d24567adc5c841c77b8d9c626531e2578aaf9fcbd754bf694d10217677396144a8d1792b09519856a2f82d20449d6d0dcdf8f5266055ec2f3d3385daadd4"}"""
    val eitherDecoded3 = JsonDecoder[NostrEvent].decodeJson(sample3)
    eitherDecoded3.isRight should be(true)
    val Right(decoded3) = eitherDecoded3
    decoded3.id should be(Sha256Digest.fromHex("77eea5982de440812d0de1656dced284671d1144ceca1764ae22cfe4745ac3af"))
    decoded3.validId should be(true)
    decoded3.pubkey should be(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"))
    decoded3.createdAt.getEpochSecond should be(1671663042)
    decoded3.kind.value should be(1)
    decoded3.kind.tags.size should be(3)
    decoded3.kind.tags(0) should be(ETag(Sha256Digest.fromHex("a4a0aad09b0f70419bce9f6f9ced24baf85c2bf32ffda9b2abda9de9e1e62d24"), None, None))
    decoded3.kind.tags(1) should be(PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"), None, None))
    decoded3.kind.tags(2) should be(PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"), None, None))
    decoded3.kind.content should be("P0ejdbB+NAzYChIE9GX6obuRVL8W70dE/zurduic0L6Qv57zPN1aC3dcHKeYAN1O?iv=L5hVzDp+2S4e/aky9fV+aQ==")
    decoded3.sig should be(NostrSignature.fromHex("ff59d24567adc5c841c77b8d9c626531e2578aaf9fcbd754bf694d10217677396144a8d1792b09519856a2f82d20449d6d0dcdf8f5266055ec2f3d3385daadd4"))
    decoded3.validSignature should be(false)
    decoded3.kind should be(a[TextNote])
    decoded3.toJson should be(sample3)

    // no tags
    val sample4 = """{"id": "0a3cd43f5dc0ce46a9f137a5dcb4b754659afcaf90caa8eaf860dd10ff845dd8","pubkey": "a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b","created_at": 1672098791,"kind": 1,"tags": [],"content": "this is a message","sig": "158c64ee17056d97765ec24dcf2723552113b66f579f2d5736d6c603d84cea16270b15ae1bfb46acc93a789402b4cbe82a5cb3f3817c1780030155523c30a5b3"}"""
    val eitherDecoded4 = JsonDecoder[NostrEvent].decodeJson(sample4)
    eitherDecoded4.isRight should be(true)
    val Right(decoded4) = eitherDecoded4
    decoded4.validId should be(true)
    decoded4.validSignature should be(true)
    decoded4.kind should be(a[TextNote])
    NostrEvent.commitment(decoded4.pubkey, decoded4.kind, decoded4.createdAt) should be("""[0,"a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b",1672098791,1,[],"this is a message"]""")

    val seckey = NostrPrivateKey.freshPrivateKey
    val pubkey = seckey.publicKey
    val event = NostrEvent.setMetadata(
      privateKey = seckey,
      createdAt = Instant.ofEpochSecond(1671663042L),
      tags =
        Vector(
          ETag(Crypto.sha256(pubkey.toByteArray), None, Some(Reply)),
          PTag(pubkey, None, None),
        ),
    )

    val encoded = event.toJson
    encoded shouldNot contain("null")
    val decoded = JsonDecoder[NostrEvent].decodeJson(encoded)

    decoded.foreach(_.kind should be(a[SetMetadata]))
    decoded.foreach(_.id should be(event.id))
    decoded.foreach(_.pubkey should be(event.pubkey))
    decoded.foreach(_.createdAt should be(event.createdAt))
    decoded.foreach(_.kind.value should be(event.kind.value))
    decoded.foreach(_.kind.content should be(event.kind.content))
    decoded.foreach(_.kind.tags should be(event.kind.tags))
    decoded.foreach(_.sig should be(event.sig))
    decoded.foreach(_.validId should be(true))
    decoded.foreach(_.validSignature should be(true))
  }
}
