package snostr.core

import snostr.core.ETag.{Marker, Reply, Root}
import snostr.core.NostrTag.{emptyStringsToNones, nonesToEmptyStrings, padWithEmptyStrings}

import java.time.Instant

sealed trait NostrTagKind {
  def value: String
}

object NostrTagKind {
  def fromString(s: String): NostrTagKind = s match {
    case A.value => A
    case D.value => D
    case E.value => E
    case G.value => G
    case I.value => I
    case P.value => P
    case R.value => R
    case T.value => T
    case Nonce.value => Nonce
    case Subject.value => Subject
    case Expiration.value => Expiration
    case Challenge.value => Challenge
    case Relay.value => Relay
    case s => CustomTagKind(s)
  }
}

case object A extends NostrTagKind {
  override val value: String = "a"
}

case object D extends NostrTagKind {
  override val value: String = "d"
}

case object E extends NostrTagKind {
  override val value: String = "e"
}

case object G extends NostrTagKind {
  override val value: String = "g"
}

case object I extends NostrTagKind {
  override val value: String = "i"
}

case object P extends NostrTagKind {
  override val value: String = "p"
}

case object R extends NostrTagKind {
  override val value: String = "r"
}

case object T extends NostrTagKind {
  override val value: String = "t"
}

case object Nonce extends NostrTagKind {
  override val value: String = "nonce"
}

case object Subject extends NostrTagKind {
  override val value: String = "subject"
}

case object Expiration extends NostrTagKind {
  override val value: String = "expiration"
}

case object Challenge extends NostrTagKind {
  override val value: String = "challenge"
}

case object Relay extends NostrTagKind {
  override val value: String = "relay"
}

case class CustomTagKind(value: String) extends NostrTagKind

sealed trait NostrTag {
  def kind: NostrTagKind

  def toStrings: Vector[String]
}

object NostrTag {
  def fromStrings(vec: Vector[String]): NostrTag = {
    require(vec.nonEmpty)
    NostrTagKind.fromString(vec.head) match {
      case D => DTag.fromStrings(vec)
      case E => ETag.fromStrings(vec)
      case P => PTag.fromStrings(vec)
      case Nonce => NonceTag.fromStrings(vec)
      case Subject => SubjectTag.fromStrings(vec)
      case Expiration => ExpirationTag.fromStrings(vec)
      case Challenge => ChallengeTag.fromStrings(vec)
      case Relay => RelayTag.fromStrings(vec)
      case _ => CustomTag.fromStrings(vec)
    }
  }

  def eTagsForReply(replyToEventId: Sha256Digest, relay: String): Vector[NostrTag] =
    eTagsForReply(replyToEventId, replyToEventId, relay)

  def eTagsForReply(replyToEventId: Sha256Digest, rootEventId: Sha256Digest, relay: String): Vector[NostrTag] = {
    val rootTag = ETag(rootEventId, Some(relay), Some(Root))
    if (rootTag.eventId == replyToEventId)
      Vector(rootTag)
    else
      Vector(rootTag, ETag(replyToEventId, Some(relay), Some(Reply)))
  }

  def pTagsForReply(replyTo: NostrEvent): Vector[NostrTag] = {
    PTag(replyTo.pubkey, None, None) +: replyTo.kind.tags.filter(_.kind == P)
  }

  private[core] def emptyStringsToNones(vec: Vector[String]): Vector[Option[String]] = vec.map(s => if (s.isEmpty) None else Some(s))

  private[core] def nonesToEmptyStrings(vec: Vector[Option[String]]): Vector[String] = dropTrailingNones(vec).map {
    case Some(value) => value
    case None => ""
  }

  private[core] def dropTrailingNones(vec: Vector[Option[String]]): Vector[Option[String]] = {
    vec.reverse.dropWhile(_.isEmpty).reverse
  }

  private[core] def padWithEmptyStrings(vec: Vector[String], length: Int): Vector[String] = {
    if (vec.length >= length) {
      vec
    } else {
      vec ++ Vector.fill(length - vec.length)("")
    }
  }
}

case class DTag(identifier: String) extends NostrTag {
  override def kind: NostrTagKind = D

  override def toStrings: Vector[String] = Vector(kind.value, identifier)
}

object DTag {
  def fromStrings(vec: Vector[String]): DTag = {
    require(vec.size == 2, "invalid e tag")
    require(vec.head == D.value, "invalid e tag")
    DTag(vec(1))
  }
}

case class ETag(eventId: Sha256Digest, recommendedRelayUrl: Option[String], marker: Option[Marker]) extends NostrTag {
  override def kind: NostrTagKind = E

  override def toStrings: Vector[String] = nonesToEmptyStrings(
    Vector(
      Some(kind.value),
      Some(eventId.toHex),
      recommendedRelayUrl,
      marker.map(_.value)))
}

object ETag {
  sealed trait Marker {
    def value: String
  }

  object Marker {
    def fromString(s: String): Marker = s match {
      case Root.value => Root
      case Reply.value => Reply
      case Mention.value => Mention
      case err => throw new IllegalArgumentException(s"unknown e tag marker: $err")
    }
  }

  case object Root extends Marker {
    override val value: String = "root"
  }

  case object Reply extends Marker {
    override val value: String = "reply"
  }

  case object Mention extends Marker {
    override val value: String = "mention"
  }

  def fromStrings(vec: Vector[String]): ETag = {
    require(vec.size >= 2 && vec.size <= 4, "invalid e tag")
    require(vec.head == E.value, "invalid e tag")
    val padded = emptyStringsToNones(padWithEmptyStrings(vec.tail.tail, 3))
    ETag(Sha256Digest.fromHex(vec(1)), padded(0), padded(1).map(Marker.fromString))
  }
}

case class PTag(pubkey: NostrPublicKey, recommendedRelayUrl: Option[String], petname: Option[String]) extends NostrTag {
  override val kind: NostrTagKind = P

  override def toStrings: Vector[String] = NostrTag.nonesToEmptyStrings(
    Vector(
      Some(kind.value),
      Some(pubkey.toHex),
      recommendedRelayUrl,
      petname))
}

object PTag {
  def fromStrings(vec: Vector[String]): PTag = {
    require(vec.size >= 2 && vec.size <= 4 && vec.head == P.value, "invalid p tag")
    val padded = emptyStringsToNones(padWithEmptyStrings(vec.tail.tail, 3))
    PTag(NostrPublicKey.fromHex(vec(1)), padded(0), padded(1))
  }
}

case class NonceTag(nonce: Int, difficulty: Int) extends NostrTag {
  override val kind: NostrTagKind = Nonce

  override def toStrings: Vector[String] = Vector(
    kind.value,
    nonce.toString,
    difficulty.toString)
}

object NonceTag {
  def fromStrings(vec: Vector[String]): NonceTag = {
    require(vec.size == 3 && vec.head == Nonce.value, "invalid nonce tag")
    NonceTag(vec(1).toInt, vec(2).toInt)
  }
}

case class SubjectTag(subject: String) extends NostrTag {
  override val kind: NostrTagKind = Subject

  override def toStrings: Vector[String] = Vector(
    kind.value,
    subject)
}

object SubjectTag {
  def fromStrings(vec: Vector[String]): SubjectTag = {
    require(vec.size == 2 && vec.head == Subject.value, "invalid subject tag")
    SubjectTag(vec(1))
  }
}

case class ExpirationTag(expiration: Instant) extends NostrTag {
  override val kind: NostrTagKind = Expiration

  override def toStrings: Vector[String] = Vector(
    kind.value,
    expiration.getEpochSecond.toString)
}

object ExpirationTag {
  def fromStrings(vec: Vector[String]): ExpirationTag = {
    require(vec.size == 2 && vec.head == Expiration.value, "invalid expiration tag")
    ExpirationTag(Instant.ofEpochSecond(vec(1).toLong))
  }
}

case class CustomTag(kind: NostrTagKind, values: Vector[String]) extends NostrTag {
  override def toStrings: Vector[String] = kind.value +: values
}

object CustomTag {
  def apply(kind: String, values: Vector[String]): CustomTag = CustomTag(CustomTagKind(kind), values)

  def fromStrings(vec: Vector[String]): CustomTag = {
    require(vec.nonEmpty, "invalid tag")
    CustomTag(kind = CustomTagKind(vec.head), values = vec.tail)
  }
}

case class ChallengeTag(challenge: String) extends NostrTag {
  override val kind: NostrTagKind = Challenge

  override def toStrings: Vector[String] = Vector(
    kind.value,
    challenge)
}

object ChallengeTag {
  def fromStrings(vec: Vector[String]): ChallengeTag = {
    require(vec.size == 2 && vec.head == Challenge.value, "invalid challenge tag")
    ChallengeTag(vec(1))
  }
}

case class RelayTag(relay: String) extends NostrTag {
  override val kind: NostrTagKind = Relay

  override def toStrings: Vector[String] = Vector(
    kind.value,
    relay)
}

object RelayTag {
  def fromStrings(vec: Vector[String]): RelayTag = {
    require(vec.size == 2 && vec.head == Relay.value, "invalid relay tag")
    RelayTag(vec(1))
  }
}
