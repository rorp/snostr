# SNOSTR

A minimalistic Scala [Nostr](https://github.com/nostr-protocol/nostr) toolkit.

It provides most of the data types defined in the NIPs in a more or less
type-safe manner. One of the main design goals was keeping the number of
dependencies as low as possible to decrease the chance of possible
conflicts of dependencies in the apps, and also to minimize the possible
attack vector.

The core module depends on [secp256k1-kmp](https://github.com/ACINQ/secp256k1-kmp)
for cryptographic primitives, and [bitcoin-kmp](https://github.com/ACINQ/bitcoin-kmp)
for some higher level data types and utilities.

Since Nostr protocol is highly JSON-centric, and there are a lot of different
JSON Scala libraries, we decoupled the JSON encoding/decoding from the Scala
data types using `Codecs` trait. `snostr` provides a few implementation of
`Codecs`, but users are welcome to implement their own codecs using their
favorite JSON libraries.

## The basic data types

### NostrEvent

`snostr` supports various kinds of Nostr events. The base class for all
events is `NostrEvent`. Its companion object contains a lot of utility
methods to create signed events. Here are some examples.

Note that these methods need an implicit `Codec` to compute SHA256 hashes of
events they create.

#### SetMetadata

NIP-01, NIP-05

```scala
import snostr.core._
import snostr.codec.jackson.JacksonCodecs

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val setMetadata = NostrEvent.setMetadata(
  privateKey = seckey,
  name = Some("Alice"),
  nip05 = Some(Nip05Identifier("alice", "domain")))
```

#### TextNote

NIP-01, NIP-10, NIP-13, NIP-14 

```scala
import snostr.core._
import snostr.codec.jackson.JacksonCodecs
import java.time.Instant
import java.time.temporal.ChronoUnit

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey
val textNote = NostrEvent.textNote(
  privateKey = seckey,
  content = "this is a message",
  subject = Some("this is a subject"),
  expiration = Some(Instant.now().plus(15, ChronoUnit.MINUTES))

val tags =
  NostrTag.eTagsForReply(textNote.id, "ws://relay") ++
    NostrTag.pTagsForReply(textNote)

val reply = NostrEvent.reply(
  privateKey = seckey,
  content = "this is a response",
  replyTo = textNote,
  extraTags = tags,
  subjectPrefix = "Re: ")
```

#### RecommendServer

NIP-01

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val recommendServer = NostrEvent.recommendServer(
  privateKey = seckey,
  url = "ws://relay")
```

#### ContactList

NIP-02

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val contacts = Vector(
  ContactList.Contact(
    publicKey = NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b"),
    mainRelayUrl = "ws:/relay",
    petname = "fido"))

val contactList = NostrEvent.contactList(
  privateKey = seckey,
  contacts = contacts)
```

#### EncryptedDirectMessage

NIP-04

```scala
import scala.util.Try
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val senderSeckey = NostrPrivateKey.freshPrivateKey
val receiverSeckey = NostrPrivateKey.freshPrivateKey
val receiverPubkey = receiverSeckey.publicKey

val encryptedDirectMessage = NostrEvent.encryptedDirectMessage(
  senderPrivateKey = senderSeckey,
  receiverPublicKey = receiverPubkey,
  content = "this is an encrypted message")

val decryptedContent = encryptedDirectMessage.kind match {
  case dm: EncryptedDirectMessage => Try(dm.decrypt(receiverSeckey)).toOption
  case _ => None
}
```

#### Deletion

NIP-09

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val eventsToDelete =
  Vector(Sha256Digest.fromHex("034a62112055611858ab9137e8b57f5ba6bbd30aaf2e17066ef82778866e877e"))

val deletion = NostrEvent.deletion(
  privateKey = seckey,
  eventIds = eventsToDelete)
```

#### Repost

NIP-18

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val textNote = NostrEvent.textNote(seckey, "this is a reposted message")

val repost = NostrEvent.repost(
  privateKey = seckey,
  event = textNote,
  relay = "ws://relay")
```

#### Reaction

NIP-25

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val textNote = NostrEvent.textNote(seckey, "this is a liked message")

val like = NostrEvent.reaction(
  privateKey = seckey,
  event = textNote,
  content = "+")
```

#### Custom event type

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val tags = Vector(
  ETag(Sha256Digest.fromHex("034a62112055611858ab9137e8b57f5ba6bbd30aaf2e17066ef82778866e877e")),
  PTag(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b")),
  CustomTag("g", Vector("geo", "location")))

val customEvent = NostrEvent.custom(
  privateKey = seckey,
  kind = 99999,
  content = "this is a custom event",
  tags = tags)
```

### NostrFilter

NIP-01, NIP-12

All fields of `NostrFilter` are optional, but you should set at least one
field to something, because Nostr relays reject empty filters.

```scala
import snostr.core._

val filter = NostrFilter(
  authors = Vector("abcdef"),
  ids = Vector("00000000"),
  kinds = Vector(1, 2, 3))

val filter1 = filter
  .withE(Vector(Sha256Digest.fromHex("034a62112055611858ab9137e8b57f5ba6bbd30aaf2e17066ef82778866e877e")))
  .withP(Vector(NostrPublicKey.fromHex("a5269a7f1b642f21f227d314bc3cc72fe25545908b1544504918023b8fb4985b")))
  .withTag(CustomTagKind("g"), Vector("geo location"))
  .withSince(0)
  .withUntil(Int.MaxValue)
  .withLimit(10)
```

### NostrClientMessage

NIP-01

`NostrClientMessage` subclasses represent Nostr client messages.

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val event = NostrEvent.textNote(
  privateKey = seckey,
  content = "this is a message")

val filter = NostrFilter(kinds = Vector(1))

val eventMessage = EventClientMessage(event)

val subscribeMessage = ReqClientMessage("subscription id", Vector(filter))

val unsubscribeMessage = CloseClientMessage("subscription id")

val messages = Vector(
  eventMessage,
  subscribeMessage,
  unsubscribeMessage
)

val encodedMessages = messages.map(codecs.encodeClientMessage)
val decodedMessages = encodedMessages.map(codecs.decodeClientMessage)
```

### NostrRelayMessage

NIP-01, NIP-15, NIP-20

`NostrRelayMessage` subclasses represent Nostr relay messages.

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._
import snostr.core.OkRelayMessage._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val event = NostrEvent.textNote(
  privateKey = seckey,
  content = "this is a message")

val eventMessage = EventRelayMessage("subscription id", event)

val noticeMessage = NoticeRelayMessage("notice")

val eoseMessage = EndOfStoredEventsRelayMessage("subscription id")

val okMessage = OkRelayMessage(event.id, Saved("message", duplicate = false))

okMessage.result match {
  case Saved(message)       => ???
  case Blocked(message)     => ???
  case Invalid(message)     => ???
  case Pow(message)         => ???
  case RateLimited(message) => ???
  case Error(message)       => ???
  case other: Rejected      => ???
}

val messages = Vector(
  eventMessage,
  noticeMessage,
  eoseMessage,
  okMessage
)

val encodedMessages = messages.map(codecs.encodeRelayMessage)
val decodedMessages = encodedMessages.map(codecs.decodeRelayMessage)
```

### Codecs

In order to implement your own version of the codecs you need to implement 
`Codecs` trait.

```scala
  object MyCodecs extends Codecs {
    override def encodeCommitment(commitment: NostrEvent.Commitment): String = ???

    override def encodeClientMessage(message: NostrClientMessage): String = ???

    override def decodeClientMessage(json: String): NostrClientMessage = ???

    override def encodeRelayMessage(message: NostrRelayMessage): String = ???

    override def decodeRelayMessage(json: String): NostrRelayMessage = ???
  }
```

### NostrClient

`NostrClient` represents one way to implement a Nostr client. There definitely
are others. `AkkaHttpNostrClient` is an example implementation of `NostrClient`,
but it does work. 

## To Apple silicon users

`secp256k1-kmp` uses native dynamically loaded libraries for different platforms
for performance reasons. Unfortunately, ACINQ does not yet support `darwin-aarch64`
architecture. [Here is their reasoning](https://github.com/ACINQ/secp256k1-kmp/pull/69)
if you are interested.

You can download an unofficial build for `darwin-aarch64` here: 
https://github.com/rorp/secp256k1-kmp-jni-jvm-darwin/blob/master/secp256k1-kmp-jni-jvm-darwin-0.7.0.jar

Create `core/lib` folder in the project root and copy `secp256k1-kmp-jni-jvm-darwin-0.7.0.jar`
in there. 

Since this is an unofficial build, please, use this JAR-file only for 
development, and always retest your code on supported architectures like 
`linux-x86_46`.
