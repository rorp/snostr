package snostr.core

import fr.acinq.bitcoin.{ByteVector, ByteVector32, ByteVector64}
import fr.acinq.secp256k1.Secp256k1

case class NostrSignature(value: ByteVector64) {
  def toHex: String = value.toHex

  def verify(sha256: Sha256Digest, publicKey: NostrPublicKey): Boolean = {
    verify(sha256.value, publicKey)
  }

  def verify(data: ByteVector, publicKey: NostrPublicKey): Boolean = {
    Secp256k1.get().verifySchnorr(value.toByteArray, data.toByteArray, publicKey.toByteArray)
  }
}

object NostrSignature {
  def fromHex(hex: String): NostrSignature = {
    NostrSignature(new ByteVector64(hex));
  }

  def sign(sha256Digest: Sha256Digest, privateKey: NostrPrivateKey): NostrSignature = {
    sign(sha256Digest.value, privateKey)
  }

  def sign(data: ByteVector, privateKey: NostrPrivateKey): NostrSignature = {
    sign(data, privateKey, Crypto.randomBytes32)
  }

  def sign(data: ByteVector, privateKey: NostrPrivateKey, auxrand: ByteVector32): NostrSignature = {
    NostrSignature(Crypto.sign(data, privateKey, auxrand))
  }
}
