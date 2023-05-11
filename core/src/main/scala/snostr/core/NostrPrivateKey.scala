package snostr.core

import fr.acinq.bitcoin.{Bech32, PrivateKey}

case class NostrPrivateKey(privateKey: PrivateKey) extends Nip19Entity {
  final override def toString: String = "<private_key>"

  def toHex: String = privateKey.value.toHex

  def toBech32: String = Bech32.encodeBytes(NostrPrivateKey.hrp, privateKey.value.toByteArray, Bech32.Encoding.Bech32)

  def toByteArray: Array[Byte] = privateKey.value.toByteArray

  def publicKey: NostrPublicKey = NostrPublicKey(privateKey.publicKey().xOnly())
}

object NostrPrivateKey {
  val hrp = "nsec"

  def freshPrivateKey: NostrPrivateKey = {
    val privateKey = new PrivateKey(Crypto.randomBytes32)
    NostrPrivateKey(privateKey)
  }

  def fromBech32(bech32: String): NostrPrivateKey = {
    val triple = Bech32.decodeBytes(bech32, false)
    require(triple.getFirst == NostrPrivateKey.hrp)
    require(triple.getThird == Bech32.Encoding.Bech32)
    val kBytes = new fr.acinq.bitcoin.ByteVector32(triple.getSecond)
    val xonly = new PrivateKey(kBytes)
    NostrPrivateKey(xonly)
  }

  def fromHex(hex: String): NostrPrivateKey = {
    val kBytes = new fr.acinq.bitcoin.ByteVector32(hex)
    val sec = new PrivateKey(kBytes)
    NostrPrivateKey(sec)
  }
}