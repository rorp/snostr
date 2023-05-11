package snostr.core

import fr.acinq.bitcoin.{Bech32, ByteVector, ByteVector32, XonlyPublicKey}
import kotlin.text.Charsets

import scala.util.control.Breaks.{break, breakable}

trait Nip19Entity {
  def toBech32: String
}

case class Note(id: Sha256Digest) extends Nip19Entity {
  override def toBech32: String = {
    Bech32.encodeBytes(Note.hrp, id.value.toByteArray, Bech32.Encoding.Bech32)
  }
}

object Note {
  val hrp = "note"

  def apply(event: NostrEvent): Note = Note(event.id)

  def fromBech32(bech32: String): Note = {
    val triple = Bech32.decodeBytes(bech32, false)
    require(triple.getFirst == hrp)
    require(triple.getThird == Bech32.Encoding.Bech32)
    Note(Sha256Digest(new ByteVector32(triple.getSecond)))
  }
}
case class NAddr(d: DTag, kind: Int, author: NostrPublicKey, relays: Vector[String]) extends Nip19Entity {
  require(kind >= 0, s"invalid event kind $kind")

  override def toBech32: String = {
    val dBytes = TLV.encodeDTag(d)
    val authorBytes = TLV.encodeAuthor(author)
    val kindBytes = TLV.encodeEventKind(kind)
    val relaysBytes = TLV.encodeRelays(relays)
    val bytes = dBytes.plus(authorBytes).plus(kindBytes).plus(relaysBytes)

    Bech32.encodeBytes(NAddr.hrp, bytes.toByteArray, Bech32.Encoding.Bech32)
  }
}

object NAddr {
  val hrp = "naddr"

  def fromBech32(bech32: String): NAddr = {
    val triple = Bech32.decodeBytes(bech32, false)
    require(triple.getFirst == hrp)
    require(triple.getThird == Bech32.Encoding.Bech32)

    val (d, tail1) = TLV.decodeDTag(new ByteVector(triple.getSecond))
    val (author, tail2) = TLV.decodeAuthor(tail1)
    val (kind, tail3) = TLV.decodeEventKind(tail2)
    val (relays, _) = TLV.decodeRelays(tail3)

    NAddr(
      d = d,
      relays = relays,
      author = author.getOrElse(throw new RuntimeException()),
      kind = kind.getOrElse(throw new RuntimeException())
    )
  }
}

case class NEvent(id: Sha256Digest, relays: Vector[String], author: Option[NostrPublicKey], kind: Option[Int]) extends Nip19Entity {
  require(kind.forall(_ >= 0), s"invalid event kind $kind")

  override def toBech32: String = {
    val idBytes = TLV.encodeId(id)
    val relayBytes = TLV.encodeRelays(relays)
    val authorBytes = author.map(TLV.encodeAuthor).getOrElse(ByteVector.empty)
    val kindBytes = kind.map(TLV.encodeEventKind).getOrElse(ByteVector.empty)
    val bytes = idBytes.plus(relayBytes).plus(authorBytes).plus(kindBytes)
    Bech32.encodeBytes(NEvent.hrp, bytes.toByteArray, Bech32.Encoding.Bech32)
  }
}

object NEvent {
  val hrp = "nevent"

  def fromBech32(bech32: String): NEvent = {
    val triple = Bech32.decodeBytes(bech32, false)
    require(triple.getFirst == hrp)
    require(triple.getThird == Bech32.Encoding.Bech32)

    val (id, tail1) = TLV.decodeEventId(new ByteVector(triple.getSecond))
    val (relays, tail2) = TLV.decodeRelays(tail1)
    val (author, tail3) = TLV.decodeAuthor(tail2)
    val (kind, _) = TLV.decodeEventKind(tail3)

    NEvent(
      id = id,
      relays = relays,
      author = author,
      kind = kind
    )
  }
}

case class NProfile(pubkey: NostrPublicKey, relays: Vector[String]) extends Nip19Entity {
  override def toBech32: String = {
    val pubkeyBytes = TLV.encodePubkey(pubkey)
    val relaysBytes = TLV.encodeRelays(relays)
    val bytes = pubkeyBytes.plus(relaysBytes)

    Bech32.encodeBytes(NProfile.hrp, bytes.toByteArray, Bech32.Encoding.Bech32)
  }
}

object NProfile {
  val hrp = "nprofile"

  def fromBech32(bech32: String): NProfile = {
    val triple = Bech32.decodeBytes(bech32, false)
    require(triple.getFirst == hrp)
    require(triple.getThird == Bech32.Encoding.Bech32)

    val (pubkey, rest) = TLV.decodePubkey(new ByteVector(triple.getSecond))
    val (relays, _) = TLV.decodeRelays(rest)

    NProfile(pubkey, relays)
  }
}

case class NRelay(relay: String) extends Nip19Entity {
  override def toBech32: String = {
    val bytes = TLV.encodeRelay(relay)
    Bech32.encodeBytes(NRelay.hrp, bytes.toByteArray, Bech32.Encoding.Bech32)
  }
}

object NRelay {
  val hrp = "nrelay"

  def fromBech32(bech32: String): NRelay = {
    val triple = Bech32.decodeBytes(bech32, false)
    require(triple.getFirst == hrp)
    require(triple.getThird == Bech32.Encoding.Bech32)
    val (relay, _) = TLV.decodeRelay(new ByteVector(triple.getSecond))
    NRelay(relay)
  }
}

private object TLV {
  val special: Byte = 0
  val relay: Byte = 1
  val author: Byte = 2
  val kind: Byte = 3

  private def decodePayload(tlv: ByteVector): (Byte, ByteVector, ByteVector) = {
    val len = tlv.get(1)
    val data = tlv.drop(2).take(len)
    require(data.size() == len, "invalid TLV data size")
    val tail = tlv.drop(2 + len)
    (tlv.get(0), data, tail)
  }

  def encodeDTag(d: DTag): ByteVector = {
    val bytes = d.identifier.getBytes(Charsets.UTF_8)
    ByteVector.empty
      .concat(TLV.special)
      .concat(bytes.length.toByte)
      .concat(bytes)
  }

  def decodeDTag(bytes: ByteVector): (DTag, ByteVector) = {
    val (kind, payload, tail) = TLV.decodePayload(bytes)
    require(TLV.special == kind)
    (DTag(new String(payload.toByteArray, Charsets.UTF_8)), tail)
  }

  def encodeId(id: Sha256Digest): ByteVector = {
    ByteVector.empty
      .concat(TLV.special)
      .concat(32.toByte)
      .concat(id.value.toByteArray)
  }

  def decodeEventId(bytes: ByteVector): (Sha256Digest, ByteVector) = {
    val (kind, idBytes, tail) = TLV.decodePayload(bytes)
    require(TLV.special == kind)
    (Sha256Digest(new ByteVector32(idBytes.toByteArray)), tail)
  }

  def encodePubkey(pubkey: NostrPublicKey): ByteVector = {
    new ByteVector(Array.empty[Byte])
      .concat(TLV.special)
      .concat(32.toByte)
      .concat(pubkey.toByteArray)
  }

  def decodePubkey(bytes: ByteVector): (NostrPublicKey, ByteVector) = {
    val (kind, payload, tail) = TLV.decodePayload(bytes)
    require(TLV.special == kind)
    (NostrPublicKey(new XonlyPublicKey(new ByteVector32(payload.toByteArray))), tail)
  }

  def encodeAuthor(author: NostrPublicKey): ByteVector = {
    ByteVector.empty
      .concat(TLV.author)
      .concat(32.toByte)
      .concat(author.xonlyPublicKey.value)
  }

  def decodeAuthor(bytes: ByteVector): (Option[NostrPublicKey], ByteVector) = {
    if (bytes.size() < 2) {
      (None, bytes)
    } else {
      val (kind, payload, tail) = TLV.decodePayload(bytes)
      if (TLV.author == kind) {
        (Some(NostrPublicKey(new XonlyPublicKey(new ByteVector32(payload.toByteArray)))), tail)
      } else {
        (None, bytes)
      }
    }
  }

  def encodeEventKind(kind: Int): ByteVector = {
    val bb = java.nio.ByteBuffer
      .allocate(4)
      .order(java.nio.ByteOrder.BIG_ENDIAN)
      .putInt(kind)
    ByteVector.empty
      .concat(TLV.kind)
      .concat(4.toByte)
      .concat(bb.array())
  }

  def decodeEventKind(bytes: ByteVector): (Option[Int], ByteVector) = {
    if (bytes.size() < 2) {
      (None, bytes)
    } else {
      val (kind, payload, tail) = TLV.decodePayload(bytes)
      if (TLV.kind == kind) {
        val bb = java.nio.ByteBuffer.allocate(4)
          .order(java.nio.ByteOrder.BIG_ENDIAN)
        bb.put(payload.toByteArray)
        val k = bb.getInt(0)
        (Some(k), tail)
      } else {
        (None, bytes)
      }
    }
  }

  def encodeRelay(relay: String): ByteVector = {
    val xs = relay.getBytes(Charsets.UTF_8)
    ByteVector.empty
      .concat(TLV.special)
      .concat(xs.length.toByte)
      .concat(xs)
  }

  def decodeRelay(bytes: ByteVector): (String, ByteVector) = {
    val (kind, payload, tail) = TLV.decodePayload(bytes)
    require(TLV.special == kind)
    (new String(payload.toByteArray, Charsets.UTF_8), tail)
  }

  def encodeRelays(relays: Vector[String]): ByteVector = {
    relays.foldLeft(ByteVector.empty) { (acc, x) =>
      val xs = x.getBytes(Charsets.UTF_8)
      acc
        .concat(TLV.relay)
        .concat(xs.length.toByte)
        .concat(xs)
    }
  }

  def decodeRelays(bytes: ByteVector): (Vector[String], ByteVector) = {
    if (bytes.size() < 2) {
      (Vector.empty, bytes)
    } else {
      decodeList(TLV.relay, bytes)(payload => new String(payload.toByteArray, Charsets.UTF_8))
    }
  }

  private def decodeList[T](kind: Byte, bytes: ByteVector)(f: ByteVector => T): (Vector[T], ByteVector) = {
    var tail = bytes
    var relays = Vector.empty[T]
    breakable {
      while (tail.size() > 0) {
        val (k, payload, rest) = TLV.decodePayload(tail)
        if (k == kind) {
          relays = relays :+ f(payload)
          tail = rest
        } else {
          break
        }
      }
    }
    (relays, tail)
  }

}
