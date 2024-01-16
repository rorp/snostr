# SNOSTR

A minimalistic Scala [Nostr](https://github.com/nostr-protocol/nostr) toolkit.

It provides most of the data types defined in the NIPs in a more or less
type-safe manner. One of the main design goals was keeping the number of
dependencies as low as possible to decrease the chance of possible
conflicts of dependencies in the apps, and also to minimize the possible
attack vector.

The core module depends on [secp256k1-kmp](https://github.com/ACINQ/secp256k1-kmp) and
[Lazysodium for Java](https://github.com/terl/lazysodium-java)
for cryptographic primitives, and [bitcoin-kmp](https://github.com/ACINQ/bitcoin-kmp)
for some higher level data types and utilities.

Since Nostr protocol is highly JSON-centric, and there are a lot of different
JSON Scala libraries, we decoupled the JSON encoding/decoding from the Scala
data types using `Codecs` trait. `snostr` provides a few implementation of
`Codecs`, but users are welcome to implement their own codecs using their
favorite JSON libraries.

## Installation

Install it by adding to your `build.sbt` these lines:

#### The core library

```sbt
libraryDependencies += "io.github.rorp" %% "snostr-core" % "0.3.0"
```

#### The codecs

```sbt
libraryDependencies += "io.github.rorp" %% "snostr-codec-jackson" % "0.3.0"
```

or 

```sbt
libraryDependencies += "io.github.rorp" %% "snostr-codec-zio-json" % "0.3.0"
```

#### The Akka HTTP client

```sbt
libraryDependencies += "io.github.rorp" %% "snostr-client-akka-http" % "0.3.0"
```


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

NIP-01, NIP-10, NIP-13, NIP-14, NIP-40

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

NIP-04, NIP-44 (experimental)

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
  content = "this is an encrypted message",
  nipNumber = 44)

val decryptedContent = encryptedDirectMessage.kind match {
  case dm: EncryptedDirectMessage => Try(dm.decryptForReceiver(receiverSeckey)).toOption
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

#### Authentication

NIP-42

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val auth = NostrEvent.authMessage(
  privateKey = seckey,
  challenge = "auth challenge",
  relay = "ws://relay/")
```

#### Zap Request

NIP-57

```scala
import snostr.codec.zio.ZioJsonCodecs
import snostr.core._

implicit val codecs = ZioJsonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val zapRequest = NostrEvent.zapRequest(
  privateKey = seckey,
  relays = Vector("wss://nostr-pub.wellorder.com", "wss://anotherrelay.example.com"),
  amount = 21000,
  lnurl = "lnurl1dp68gurn8ghj7um5v93kketj9ehx2amn9uh8wetvdskkkmn0wahz7mrww4excup0dajx2mrv92x9xp",
  recipient = NostrPublicKey.fromHex("04c915daefee38317fa734444acee390a8269fe5810b2241e5e6dd343dfbecc9"),
  content = Some("Zap!"),
  eventId = Some(Sha256Digest.fromHex("9ae37aa68f48645127299e9453eb5d908a0cbb6058ff340d528ed4d37c8994fb")),
)
```

#### Zap Receipt

NIP-57

```scala
import snostr.codec.zio.ZioJsonCodecs
import snostr.core._
import java.time.Instant

implicit val codecs = ZioJsonCodecs

val seckey = NostrPrivateKey.freshPrivateKey
val paidAt = Instant.now()

val zapReceipt = NostrEvent.zapReceipt(
  privateKey = seckey,
  recipient = NostrPublicKey.fromHex("32e1827635450ebb3c5a7d12c1f8e7b2b514439ac10a67eef3d9fd9c5c68e245"),
  bolt11 = "lnbc10u1p3unwfusp5t9r3yymhpfqculx78u027lxspgxcr2n2987mx2j55nnfs95nxnzqpp5jmrh92pfld78spqs78v9euf2385t83uvpwk9ldrlvf6ch7tpascqhp5zvkrmemgth3tufcvflmzjzfvjt023nazlhljz2n9hattj4f8jq8qxqyjw5qcqpjrzjqtc4fc44feggv7065fqe5m4ytjarg3repr5j9el35xhmtfexc42yczarjuqqfzqqqqqqqqlgqqqqqqgq9q9qxpqysgq079nkq507a5tw7xgttmj4u990j7wfggtrasah5gd4ywfr2pjcn29383tphp4t48gquelz9z78p4cq7ml3nrrphw5w6eckhjwmhezhnqpy6gyf0",
  description = "{\"pubkey\":\"97c70a44366a6535c145b333f973ea86dfdc2d7a99da618c40c64705ad98e322\",\"content\":\"\",\"id\":\"d9cc14d50fcb8c27539aacf776882942c1a11ea4472f8cdec1dea82fab66279d\",\"created_at\":1674164539,\"sig\":\"77127f636577e9029276be060332ea565deaf89ff215a494ccff16ae3f757065e2bc59b2e8c113dd407917a010b3abd36c8d7ad84c0e3ab7dab3a0b0caa9835d\",\"kind\":9734,\"tags\":[[\"e\",\"3624762a1274dd9636e0c552b53086d70bc88c165bc4dc0f9e836a1eaf86c3b8\"],[\"p\",\"32e1827635450ebb3c5a7d12c1f8e7b2b514439ac10a67eef3d9fd9c5c68e245\"],[\"relays\",\"wss://relay.damus.io\",\"wss://nostr-relay.wlvs.space\",\"wss://nostr.fmt.wiz.biz\",\"wss://relay.nostr.bg\",\"wss://nostr.oxtr.dev\",\"wss://nostr.v0l.io\",\"wss://brb.io\",\"wss://nostr.bitcoiner.social\",\"ws://monad.jb55.com:8080\",\"wss://relay.snort.social\"]]}",
  preimage = Some("5d006d2cf1e73c7148e7519a4c68adc81642ce0e25a432b2434c99f97344c15f"),
  eventId = Some(Sha256Digest.fromHex("3624762a1274dd9636e0c552b53086d70bc88c165bc4dc0f9e836a1eaf86c3b8")),
  aTag = None,
  sender = Some(NostrPublicKey.fromHex("97c70a44366a6535c145b333f973ea86dfdc2d7a99da618c40c64705ad98e322")),
  createdAt = paidAt
)
```

#### Gift Wrap

NIP-59 (experimental)

```scala
import snostr.codec.zio.ZioJsonCodecs
import snostr.core._
import scala.util.Try

implicit val codecs = ZioJsonCodecs

val senderSeckey = NostrPrivateKey.freshPrivateKey
val giftWrapSeckey = NostrPrivateKey.freshPrivateKey

val receiverSeckey = NostrPrivateKey.freshPrivateKey
val receiverPubkey = receiverSeckey.publicKey

val wrappedEvent = NostrEvent.textNote(
  privateKey = senderSeckey,
  content = "this is a wrapped message")

val giftWrap = NostrEvent.giftWrap(
  senderPrivateKey = giftWrapSeckey,
  receiverPublicKey = receiverPubkey,
  event = wrappedEvent)

val unwrappedEvent = giftWrap.kind match {
  case gw: GiftWrap => Try(gw.decryptForReceiver(receiverSeckey)).toOption
  case _ => None
}
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

NIP-01, NIP-42, NIP-45

`NostrClientMessage` subclasses represent Nostr client messages.

```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val seckey = NostrPrivateKey.freshPrivateKey

val event = NostrEvent.textNote(
  privateKey = seckey,
  content = "this is a message")

val auth = NostrEvent.authMessage(
  challenge = "auth challenge",
  relay = "wss://nostr.relay"
)

val filter = NostrFilter(kinds = Vector(1))

val eventMessage = EventClientMessage(event)

val authMessage = AuthClientMessage(event)

val subscribeMessage = ReqClientMessage("subscription id", Vector(filter))

val countMessage = CountClientMessage("subscription id", Vector(filter))

val unsubscribeMessage = CloseClientMessage("subscription id")

val messages = Vector(
  authMessage,
  eventMessage,
  subscribeMessage,
  unsubscribeMessage
)

val encodedMessages = messages.map(codecs.encodeClientMessage)
val decodedMessages = encodedMessages.map(codecs.decodeClientMessage)
```

### NostrRelayMessage

NIP-01, NIP-15, NIP-20, NIP-42, NIP-45

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
  
val count = 12345  

val eventMessage = EventRelayMessage("subscription id", event)

val countMessage = CountRelayMessage("subscription id", count)

val authMessage = AutRelayMessage("auth challenge")

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
  authMessage,
  eventMessage,
  noticeMessage,
  eoseMessage,
  okMessage
)

val encodedMessages = messages.map(codecs.encodeRelayMessage)
val decodedMessages = encodedMessages.map(codecs.decodeRelayMessage)
```

### NostrRelayInformation
NIP-11
```scala
import snostr.codec.jackson.JacksonCodecs
import snostr.core._

implicit val codecs = JacksonCodecs

val relayInfo = NostrRelayInformation(supportedNips = Vector(1, 2, 9, 11, 12, 15, 16, 20, 22))

if (relayInfo.supports(2, 9, 20)) {
  // ok
}

val encodedInfo = codecs.encodeRelayInfo
val decodedInfo = codecs.decodeRelayInfo
```


### Codecs

In order to implement your own version of the codecs you need to implement 
`Codecs` trait.

```scala
  object MyCodecs extends Codecs {
    override def encodeCommitment(commitment: NostrEvent.Commitment): String = ???
  
    override def encodeNostrEvent(message: NostrEvent): String

    override def decodeNostrEvent(json: String): NostrEvent
  
    override def encodeClientMessage(message: NostrClientMessage): String = ???

    override def decodeClientMessage(json: String): NostrClientMessage = ???

    override def encodeRelayMessage(message: NostrRelayMessage): String = ???

    override def decodeRelayMessage(json: String): NostrRelayMessage = ???
  
    override def encodeRelayInfo(info: NostrRelayInformation): String = ???

    override def decodeRelayInfo(json: String): NostrRelayInformation = ???
  }
```

### NostrClient

`NostrClient` represents one way to implement a Nostr client. There definitely
are others. `AkkaHttpNostrClient` is an example implementation of `NostrClient`,
but it does work. 
