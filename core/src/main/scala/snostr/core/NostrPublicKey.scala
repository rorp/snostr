package snostr.core

import fr.acinq.bitcoin.{Bech32, ByteVector32, PublicKey, XonlyPublicKey}

case class NostrPublicKey(xonlyPublicKey: XonlyPublicKey) extends Nip19Entity {
  def publicKey: PublicKey = new PublicKey(0x02.toByte +: toByteArray)

  def toByteArray: Array[Byte] = xonlyPublicKey.value.toByteArray

  def toBech32: String = Bech32.encodeBytes(NostrPublicKey.hrp, xonlyPublicKey.value.toByteArray, Bech32.Encoding.Bech32)

  def toHex: String = xonlyPublicKey.value.toHex
}

object NostrPublicKey {
  val hrp = "npub"

  def fromBech32(bech32: String): NostrPublicKey = {
    val triple = Bech32.decodeBytes(bech32, false)
    require(triple.getFirst == NostrPublicKey.hrp)
    require(triple.getThird == Bech32.Encoding.Bech32)
    val kBytes = new ByteVector32(triple.getSecond)
    val xonly = new XonlyPublicKey(kBytes)
    NostrPublicKey(xonly)
  }

  def fromHex(hex: String): NostrPublicKey = {
    val kBytes = new ByteVector32(hex)
    val xonly = new XonlyPublicKey(kBytes)
    NostrPublicKey(xonly)
  }
}
