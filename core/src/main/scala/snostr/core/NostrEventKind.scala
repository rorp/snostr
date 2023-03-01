package snostr.core

import snostr.core.ETag.Mention

object NostrEventKindCodes {
  val SetMetadata = 0
  val TextNote = 1
  val RecommendServer = 2
  val ContactList = 3
  val EncryptedDirectMessage = 4
  val Deletion = 5
  val Repost = 6
  val Reaction = 7
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

case class EncryptedDirectMessage(override val content: String, receiverPublicKey: NostrPublicKey, senderPublicKey: NostrPublicKey, extraTags: Vector[NostrTag] = Vector.empty, parsedTags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.EncryptedDirectMessage

  override def computedContent: String = content

  override def computedTags: Vector[NostrTag] = Vector(PTag(receiverPublicKey, None, None)) ++ extraTags

  def decryptForReceiver(receiverPrivateKey: NostrPrivateKey): String =
    Crypto.decryptDirectMessage(receiverPrivateKey, senderPublicKey, content)

  def decryptForSender(senderPrivateKey: NostrPrivateKey): String =
    Crypto.decryptDirectMessage(senderPrivateKey, receiverPublicKey, content)
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