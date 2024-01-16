package snostr.core

import java.nio.charset.Charset
import java.time.Instant

case class NostrEvent(id: Sha256Digest,
                      pubkey: NostrPublicKey,
                      createdAt: Instant,
                      sig: NostrSignature,
                      kind: NostrEventKind) {
  def content: String = kind.content

  def tags: Vector[NostrTag] = kind.tags

  def validId(implicit codecs: Codecs): Boolean = id == hash

  def hash(implicit codecs: Codecs): Sha256Digest = NostrEvent.sha256(pubkey, kind, createdAt)

  def commitment(implicit codecs: Codecs): String = NostrEvent.commitment(pubkey, kind, createdAt)

  def validSignature: Boolean = sig.verify(id, pubkey)
}

object NostrEvent {
  type Commitment = (Int, NostrPublicKey, Long, Int, Vector[NostrTag], String)
  private val UTF8 = Charset.forName("UTF-8")

  def setMetadata(privateKey: NostrPrivateKey,
                  name: Option[String] = None,
                  nip05: Option[Nip05Identifier] = None,
                  about: Option[String] = None,
                  picture: Option[String] = None,
                  extraMetadata: Map[String, String] = Map(),
                  tags: Vector[NostrTag] = Vector(),
                  expiration: Option[Instant] = None,
                  createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = SetMetadata.build(
      name = name,
      nip05 = nip05,
      about = about,
      picture = picture,
      extraMetadata = extraMetadata,
      tags = tags ++ expirationTag(expiration))
    signedEvent(privateKey, eventKind, createdAt)
  }

  def textNote(privateKey: NostrPrivateKey,
               content: String,
               tags: Vector[NostrTag] = Vector.empty,
               subject: Option[String] = None,
               expiration: Option[Instant] = None,
               createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val stag = Vector(subject.map(SubjectTag(_))).flatten
    val eventKind = TextNote(
      content = content,
      tags = stag ++ tags ++ expirationTag(expiration))
    signedEvent(privateKey, eventKind, createdAt)
  }

  def recommendServer(privateKey: NostrPrivateKey,
                      url: String,
                      tags: Vector[NostrTag] = Vector.empty,
                      expiration: Option[Instant] = None,
                      createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = RecommendServer(
      url = url,
      tags = tags ++ expirationTag(expiration))
    signedEvent(privateKey, eventKind, createdAt)
  }

  def contactList(privateKey: NostrPrivateKey,
                  contacts: Vector[ContactList.Contact],
                  extraTags: Vector[NostrTag] = Vector.empty,
                  expiration: Option[Instant] = None,
                  createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = ContactList(
      contacts = contacts,
      extraTags = extraTags ++ expirationTag(expiration))
    signedEvent(privateKey, eventKind, createdAt)
  }

  def encryptedDirectMessage(senderPrivateKey: NostrPrivateKey,
                             content: String,
                             receiverPublicKey: NostrPublicKey,
                             tags: Vector[NostrTag] = Vector.empty,
                             expiration: Option[Instant] = None,
                             createdAt: Instant = Instant.now(),
                             nipNumber: Int = 44)(implicit codecs: Codecs): NostrEvent = {
    val eventKind = nipNumber match {
      case 4 =>
        EncryptedDirectMessage04(
          content = Crypto.encryptDirectMessageAES(senderPrivateKey, receiverPublicKey, content),
          receiverPublicKey = receiverPublicKey,
          senderPublicKey = senderPrivateKey.publicKey,
          extraTags = tags ++ expirationTag(expiration)
        )
      case 44 =>
        val encrypted = Crypto.encryptDirectMessageXChaCha20(senderPrivateKey, receiverPublicKey, content)
        EncryptedDirectMessage44(
          content = s"${encrypted.version},${encrypted.nonceBase64},${encrypted.ciphertextBase64}",
          receiverPublicKey = receiverPublicKey,
          senderPublicKey = senderPrivateKey.publicKey,
          extraTags = tags ++ expirationTag(expiration)
        )
      case err => throw new IllegalArgumentException(s"unsupported NIP number $err")
    }
    signedEvent(senderPrivateKey, eventKind, createdAt)
  }

  def deletion(privateKey: NostrPrivateKey,
               eventIds: Vector[Sha256Digest],
               content: String = "",
               createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = Deletion(content, eventIds)
    signedEvent(privateKey, eventKind, createdAt)
  }

  def repost(privateKey: NostrPrivateKey,
             event: NostrEvent,
             relay: String,
             createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = Repost(
      eventId = event.id,
      relay = relay,
      authorPublicKey = event.pubkey,
    )
    signedEvent(privateKey, eventKind, createdAt)
  }

  def reaction(privateKey: NostrPrivateKey,
               content: String,
               event: NostrEvent,
               createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = Reaction(
      content = content,
      eventId = event.id,
      author = event.pubkey,
      extraTags = event.kind.tags.filter(t => t.kind == E || t.kind == P)
    )
    signedEvent(privateKey, eventKind, createdAt)
  }

  def giftWrap(senderPrivateKey: NostrPrivateKey,
               event: NostrEvent,
               receiverPublicKey: NostrPublicKey,
               expiration: Option[Instant] = None,
               createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val content = codecs.encodeNostrEvent(event)
    val encrypted = Crypto.encryptDirectMessageXChaCha20(senderPrivateKey, receiverPublicKey, content)
    val eventKind = GiftWrap(
      wrappedContent = encrypted,
      receiverPublicKey = receiverPublicKey,
      senderPublicKey = senderPrivateKey.publicKey,
      extraTags = expirationTag(expiration)
    )
    signedEvent(senderPrivateKey, eventKind, createdAt)
  }

  def authMessage(privateKey: NostrPrivateKey,
                  challenge: String,
                  relay: String,
                  createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = Auth(challenge, relay)
    signedEvent(privateKey, eventKind, createdAt)
  }

  def zapRequest(privateKey: NostrPrivateKey,
                 relays: Vector[String],
                 amount: Long,
                 lnurl: String,
                 recipient: NostrPublicKey,
                 content: Option[String] = None,
                 eventId: Option[Sha256Digest] = None,
                 aTag: Option[ATag] = None,
                 createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = ZapRequest(relays, amount, lnurl, recipient, eventId, aTag, computedContent = content.getOrElse(""))
    signedEvent(privateKey, eventKind, createdAt)
  }

  def zapReceipt(privateKey: NostrPrivateKey,
                 recipient: NostrPublicKey,
                 bolt11: String,
                 description: String,
                 preimage: Option[String] = None,
                 eventId: Option[Sha256Digest] = None,
                 aTag: Option[ATag] = None,
                 sender: Option[NostrPublicKey] = None,
                 createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = ZapReceipt(recipient, bolt11, description, preimage, eventId, aTag, sender)
    signedEvent(privateKey, eventKind, createdAt)
  }

  def custom(privateKey: NostrPrivateKey,
             kind: Int,
             content: String,
             tags: Vector[NostrTag] = Vector.empty,
             expiration: Option[Instant] = None,
             createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val eventKind = Custom(
      value = kind,
      content = content,
      tags = tags ++ expirationTag(expiration))
    signedEvent(privateKey, eventKind, createdAt)
  }

  def reply(privateKey: NostrPrivateKey,
            content: String,
            replyTo: NostrEvent,
            extraTags: Vector[NostrTag] = Vector.empty,
            subjectPrefix: String = "",
            expiration: Option[Instant] = None,
            createdAt: Instant = Instant.now())(implicit codecs: Codecs): NostrEvent = {
    val subject = Vector(replyTo.kind.tags.collectFirst {
      case stag: SubjectTag =>
        if (subjectPrefix.isEmpty)
          stag
        else
          SubjectTag(subjectPrefix + stag.subject)
    }).flatten
    val tags = subject ++ extraTags ++ expirationTag(expiration)
    textNote(privateKey, content, tags, createdAt = createdAt)
  }

  def sha256(publicKey: NostrPublicKey, kind: NostrEventKind, createdAt: Instant)(implicit codecs: Codecs): Sha256Digest = {
    Crypto.sha256(commitment(publicKey, kind, createdAt)(codecs).getBytes(UTF8))
  }

  def commitment(publicKey: NostrPublicKey, kind: NostrEventKind, createdAt: Instant)(implicit codecs: Codecs): String =
    codecs.encodeCommitment((0, publicKey, createdAt.getEpochSecond, kind.value, kind.tags, kind.content))

  private def signedEvent(privateKey: NostrPrivateKey,
                          eventKind: NostrEventKind,
                          createdAt: Instant)(implicit codecs: Codecs): NostrEvent = {
    val pubkey = privateKey.publicKey
    val id = sha256(pubkey, eventKind, createdAt)
    val sig = NostrSignature.sign(id, privateKey)
    NostrEvent(id, pubkey, createdAt, sig, eventKind)
  }

  private def expirationTag(expiration: Option[Instant]): Vector[ExpirationTag] = {
    expiration.map(e => Vector(ExpirationTag(e))).getOrElse(Vector.empty)
  }
}
