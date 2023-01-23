package snostr.core

import fr.acinq.bitcoin.ByteVector32

case class Sha256Digest(value: ByteVector32) {
  def toHex: String = value.toHex
}

object Sha256Digest {
  def fromHex(hex: String): Sha256Digest = {
    Sha256Digest(ByteVector32.fromValidHex(hex))
  }
}
