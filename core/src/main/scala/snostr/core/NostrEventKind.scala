package snostr.core

import snostr.core.Crypto.EncryptedContent
import snostr.core.ETag.Mention

object NostrEventKindCodes {
  val SetMetadata = 0
  val TextNote = 1
  val RecommendServer = 2
  val ContactList = 3
  val EncryptedDirectMessage04 = 4
  val Deletion = 5
  val Repost = 6
  val Reaction = 7
  val EncryptedDirectMessage44 = 44
  val GiftWrap = 1059
  val ZapRequest = 9734
  val ZapReceipt = 9735
  val Auth = 22242
}

sealed trait NostrEventKind {
  def value: Int

  def content: String = parsedContent match {
    case Some(parsed) => parsed
    case None => computedContent
  }

  def tags: Vector[NostrTag] =
    if (parsedTags.nonEmpty)
      parsedTags
    else
      computedTags

  def computedTags: Vector[NostrTag]

  def parsedTags: Vector[NostrTag]

  def computedContent: String = ""

  def parsedContent: Option[String] = None
}

case class SetMetadata(metadata: Map[String, String], override val parsedContent: Option[String] = None, extraTags: Vector[NostrTag] = Vector.empty, parsedTags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override def value: Int = NostrEventKindCodes.SetMetadata

  import Codecs.quote

  override def computedTags: Vector[NostrTag] = extraTags

  override def computedContent: String = "{" + metadata.toSeq.sortBy(_._1).map { case (k, v) => s"\"${quote(k)}\":\"${quote(v)}\"" }.mkString(",") + "}"

  def about: Option[String] = metadata.get("about")

  def name: Option[String] = metadata.get("name")

  def nip05: Option[Nip05Identifier] = metadata.get("nip05").flatMap { s =>
    s.split("@") match {
      case Array(local, domain) => Some(Nip05Identifier(local.toLowerCase(), domain.toLowerCase()))
      case _ => None
    }
  }

  def picture: Option[String] = metadata.get("picture")
}

object SetMetadata {
  def build(name: Option[String] = None,
            nip05: Option[Nip05Identifier] = None,
            about: Option[String] = None,
            picture: Option[String] = None,
            extraMetadata: Map[String, String] = Map(),
            originalContent: Option[String] = None,
            tags: Vector[NostrTag] = Vector()): SetMetadata = {
    val metadata = List(
      name.map(("name", _)),
      about.map(("about", _)),
      picture.map(("picture", _)),
      nip05.map(n => ("nip05", n.toString))
    ).flatten.toMap ++ extraMetadata

    SetMetadata(metadata, originalContent, tags)
  }
}

case class TextNote(override val content: String, override val tags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.TextNote

  lazy val subject: Option[String] = tags.collectFirst {
    case tag: SubjectTag => tag.subject
  }

  override def computedContent: String = content

  override def computedTags: Vector[NostrTag] = tags

  override def parsedTags: Vector[NostrTag] = tags
}

case class RecommendServer(url: String, override val tags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.RecommendServer

  override def computedContent: String = url

  override def computedTags: Vector[NostrTag] = tags

  override def parsedTags: Vector[NostrTag] = tags
}

case class ContactList(contacts: Vector[ContactList.Contact], override val content: String = "", extraTags: Vector[NostrTag] = Vector.empty, parsedTags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.ContactList

  override def computedContent: String = content

  override def computedTags: Vector[NostrTag] = contacts.map(c => PTag(c.publicKey, Some(c.mainRelayUrl), Some(c.petname))) ++ extraTags
}

object ContactList {
  case class Contact(publicKey: NostrPublicKey, mainRelayUrl: String, petname: String)
}

sealed trait EncryptedDirectMessage extends NostrEventKind {
  def receiverPublicKey: NostrPublicKey

  def senderPublicKey: NostrPublicKey

  def extraTags: Vector[NostrTag] = Vector.empty

  override def computedContent: String = content

  override def computedTags: Vector[NostrTag] = Vector(PTag(receiverPublicKey, None, None)) ++ extraTags

  def decryptForReceiver(receiverPrivateKey: NostrPrivateKey): String

  def decryptForSender(senderPrivateKey: NostrPrivateKey): String
}


case class EncryptedDirectMessage04(override val content: String, override val receiverPublicKey: NostrPublicKey, override val senderPublicKey: NostrPublicKey, override val extraTags: Vector[NostrTag] = Vector.empty, parsedTags: Vector[NostrTag] = Vector.empty) extends EncryptedDirectMessage {
  override val value: Int = NostrEventKindCodes.EncryptedDirectMessage04

  override def decryptForReceiver(receiverPrivateKey: NostrPrivateKey): String =
    Crypto.decryptDirectMessageAES(receiverPrivateKey, senderPublicKey, content)

  override def decryptForSender(senderPrivateKey: NostrPrivateKey): String =
    Crypto.decryptDirectMessageAES(senderPrivateKey, receiverPublicKey, content)
}

case class Deletion(override val content: String, eventIds: Vector[Sha256Digest], parsedTags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  require(eventIds.nonEmpty, "invalid deletion message")

  override def value: Int = NostrEventKindCodes.Deletion

  override def computedContent: String = content

  override def computedTags: Vector[NostrTag] = eventIds.map(id => ETag(id, None, None))
}

case class Repost(eventId: Sha256Digest, relay: String, authorPublicKey: NostrPublicKey, parsedTags: Vector[NostrTag] = Vector.empty, override val parsedContent: Option[String] = None) extends NostrEventKind {
  override def value: Int = NostrEventKindCodes.Repost

  override def computedContent: String = ""

  override def computedTags: Vector[NostrTag] = Vector(
    ETag(eventId, Some(relay), Some(Mention)),
    PTag(authorPublicKey, None, None)
  )
}

case class Reaction(override val content: String, eventId: Sha256Digest, author: NostrPublicKey, extraTags: Vector[NostrTag] = Vector.empty, parsedTags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override def value: Int = NostrEventKindCodes.Reaction

  override def computedContent: String = content

  override def computedTags: Vector[NostrTag] = extraTags ++ Vector(
    ETag(eventId, None, None),
    PTag(author, None, None)
  )
}

case class EncryptedDirectMessage44(override val content: String, override val receiverPublicKey: NostrPublicKey, override val senderPublicKey: NostrPublicKey, override val extraTags: Vector[NostrTag] = Vector.empty, parsedTags: Vector[NostrTag] = Vector.empty) extends EncryptedDirectMessage {
  override val value: Int = NostrEventKindCodes.EncryptedDirectMessage44

  override def decryptForReceiver(receiverPrivateKey: NostrPrivateKey): String =
    Crypto.decryptDirectMessageXChaCha20(receiverPrivateKey, senderPublicKey, content)

  override def decryptForSender(senderPrivateKey: NostrPrivateKey): String =
    Crypto.decryptDirectMessageXChaCha20(senderPrivateKey, receiverPublicKey, content)
}

case class GiftWrap(wrappedContent: EncryptedContent, receiverPublicKey: NostrPublicKey, senderPublicKey: NostrPublicKey, extraTags: Vector[NostrTag] = Vector.empty, override val  parsedContent: Option[String] = None, override val parsedTags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.GiftWrap

  override def computedContent: String = s"""{"ciphertext":"${wrappedContent.ciphertextBase64}","nonce":"${wrappedContent.nonceBase64}","version":${wrappedContent.version}}"""

  override def computedTags: Vector[NostrTag] = Vector(PTag(receiverPublicKey, None, None)) ++ extraTags

  def decryptForReceiver(receiverPrivateKey: NostrPrivateKey)(implicit codecs: Codecs): NostrEvent = {
    val decryptedContent = Crypto.decryptDirectMessageXChaCha20(receiverPrivateKey, senderPublicKey, wrappedContent)
    codecs.decodeNostrEvent(decryptedContent)
  }

  def decryptForSender(senderPrivateKey: NostrPrivateKey)(implicit codecs: Codecs): NostrEvent = {
    val decryptedContent = Crypto.decryptDirectMessageXChaCha20(senderPrivateKey, receiverPublicKey, wrappedContent)
    codecs.decodeNostrEvent(decryptedContent)
  }
}

case class ZapRequest(relays: Vector[String], amount: Long, lnurl: String, recipient: NostrPublicKey, eventId: Option[Sha256Digest], aTag: Option[ATag], override val computedContent: String, parsedTags: Vector[NostrTag] = Vector.empty, override val parsedContent: Option[String] = None) extends NostrEventKind {
  override def value: Int = NostrEventKindCodes.ZapRequest

  override def computedTags: Vector[NostrTag] = {
    val tags = Vector(
      RelaysTag(relays),
      AmountTag(amount),
      LNURLTag(lnurl),
      PTag(recipient)
    )
    val optionalTags = Vector(eventId.map(ETag(_)), aTag).flatten
    tags ++ optionalTags
  }
}

case class ZapReceipt(recipient: NostrPublicKey, bolt11: String, description: String, preimage: Option[String], eventId: Option[Sha256Digest], aTag: Option[ATag], sender: Option[NostrPublicKey], parsedTags: Vector[NostrTag] = Vector.empty, override val parsedContent: Option[String] = None) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.ZapReceipt

  override def computedTags: Vector[NostrTag] = Vector(
    Some(PTag(recipient)),
    sender.map(BigPTag.apply),
    eventId.map(ETag.apply),
    aTag,
    Some(Bolt11Tag(bolt11)),
    Some(DescriptionTag(description)),
    preimage.map(PreimageTag.apply)
  ).flatten
}

case class Auth(challenge: String, relay: String, parsedTags: Vector[NostrTag] = Vector.empty, override val parsedContent: Option[String] = None) extends NostrEventKind {
  override def value: Int = NostrEventKindCodes.Auth

  override def computedContent: String = ""

  override def computedTags: Vector[NostrTag] = Vector(ChallengeTag(challenge), RelayTag(relay))
}

case class Custom(value: Int, override val content: String, override val tags: Vector[NostrTag]) extends NostrEventKind {
  override def computedContent: String = content

  override def parsedContent: Option[String] = Some(content)

  override def computedTags: Vector[NostrTag] = tags

  override def parsedTags: Vector[NostrTag] = tags
}