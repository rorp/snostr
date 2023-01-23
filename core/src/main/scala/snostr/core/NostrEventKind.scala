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
}

sealed trait NostrEventKind {
  def value: Int

  def content: String

  def tags: Vector[NostrTag]
}

case class SetMetadata(metadata: Map[String, String], originalContent: Option[String] = None, tags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override def value: Int = NostrEventKindCodes.SetMetadata

  import Codecs.quote

  override def content: String = originalContent match {
    case Some(c) => c
    case None => "{" + metadata.toSeq.sortBy(_._1).map { case (k, v) => s"\"${quote(k)}\":\"${quote(v)}\"" }.mkString(",") + "}"
  }

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

case class TextNote(content: String, tags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.TextNote

  lazy val subject: Option[String] = tags.collectFirst {
    case tag: SubjectTag => tag.subject
  }
}

case class RecommendServer(url: String, tags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.RecommendServer

  def content: String = url
}

case class ContactList(contacts: Vector[ContactList.Contact], content: String = "", extraTags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.ContactList

  override def tags: Vector[NostrTag] = contacts.map(c => PTag(c.publicKey, Some(c.mainRelayUrl), Some(c.petname)))
}

object ContactList {
  case class Contact(publicKey: NostrPublicKey, mainRelayUrl: String, petname: String)
}

case class EncryptedDirectMessage(content: String, receiverPublicKey: NostrPublicKey, senderPublicKey: NostrPublicKey, extraTags: Vector[NostrTag] = Vector.empty) extends NostrEventKind {
  override val value: Int = NostrEventKindCodes.EncryptedDirectMessage

  override def tags: Vector[NostrTag] = Vector(PTag(receiverPublicKey, None, None)) ++ extraTags

  def decrypt(receiverPrivateKey: NostrPrivateKey): String =
    Crypto.decryptDirectMessage(receiverPrivateKey, senderPublicKey, content)
}

case class Deletion(content: String, eventIds: Vector[Sha256Digest]) extends NostrEventKind {
  require(eventIds.nonEmpty, "invalid deletion message")

  override def value: Int = NostrEventKindCodes.Deletion

  override def tags: Vector[NostrTag] = eventIds.map(id => ETag(id, None, None))
}

case class Repost(eventId: Sha256Digest, relay: String, authorPublicKey: NostrPublicKey) extends NostrEventKind {
  override lazy val tags: Vector[NostrTag] = Vector(
    ETag(eventId, Some(relay), Some(Mention)),
    PTag(authorPublicKey, None, None)
  )

  override def value: Int = NostrEventKindCodes.Repost

  override def content: String = ""
}

case class Reaction(content: String, eventId: Sha256Digest, author: NostrPublicKey, extraTags: Vector[NostrTag]) extends NostrEventKind {
  override lazy val tags: Vector[NostrTag] = extraTags ++ Vector(
    ETag(eventId, None, None),
    PTag(author, None, None)
  )

  override def value: Int = NostrEventKindCodes.Reaction
}

case class Custom(value: Int, content: String, tags: Vector[NostrTag]) extends NostrEventKind