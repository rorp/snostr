package snostr.core

import fr.acinq.bitcoin.{ByteVector, ByteVector32, ByteVector64}
import fr.acinq.secp256k1.Secp256k1
import kotlin.text.Charsets

import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

object Crypto {
  private val secureRandom = new SecureRandom()
  private val AES_CBC = "AES/CBC/PKCS5Padding"
  private val AES = "AES"
  private val SHA256 = "SHA-256"

  def randomBytes(n: Int): ByteVector = new ByteVector(randomByteArray(n))

  def randomBytes32: ByteVector32 = new ByteVector32(randomByteArray(32))

  def randomByteArray(n: Int): Array[Byte] = {
    val bytes = Array.ofDim[Byte](n)
    secureRandom.nextBytes(bytes)
    bytes
  }

  def sha256(data: Array[Byte]): Sha256Digest = {
    val digest = MessageDigest.getInstance(SHA256)
    val bytes = digest.digest(data)
    Sha256Digest(new ByteVector32(bytes))
  }

  def sha256(data: ByteVector): Sha256Digest = sha256(data.toByteArray)

  def sign(data: ByteVector, privateKey: NostrPrivateKey, auxrand: ByteVector32): ByteVector64 = {
    val secp = Secp256k1.get()
    val sigBytes = secp.signSchnorr(data.toByteArray, privateKey.toByteArray, auxrand.toByteArray)
    new ByteVector64(sigBytes)
  }

  def encryptDirectMessage(ourPrivateKey: NostrPrivateKey, theirPublicKey: NostrPublicKey, content: String): String = {
    val key = sharedSecret(ourPrivateKey, theirPublicKey)
    val iv = randomByteArray(16)
    val encrypted = encryptAES(content.getBytes(Charsets.UTF_8), key, iv)
    val encryptedBase64 = toBase64(encrypted)
    val ivBase64 = toBase64(iv)
    encryptedBase64 + "?iv=" + ivBase64
  }

  def decryptDirectMessage(ourPrivateKey: NostrPrivateKey, theirPublicKey: NostrPublicKey, content: String): String = {
    val key = sharedSecret(ourPrivateKey, theirPublicKey)
    val (msg, iv) = content.split("\\?iv=") match {
      case Array(msgBase64, ivBase64) => (fromBase64(msgBase64), fromBase64(ivBase64))
      case _ => throw new IllegalArgumentException("invalid content")
    }
    val decrypted = decryptAES(msg, key, iv)
    new String(decrypted, Charsets.UTF_8)
  }

  def toBase64(data: Array[Byte]): String = Base64.getEncoder.encodeToString(data)

  def fromBase64(base64: String): Array[Byte] = Base64.getDecoder.decode(base64)

  private def sharedSecret(privateKey: NostrPrivateKey, publicKey: NostrPublicKey): Array[Byte] = {
    val secp = Secp256k1.get()
    secp.pubKeyTweakMul(publicKey.publicKey.value.toByteArray, privateKey.toByteArray).slice(1, 33)
  }

  private def encryptAES(messageBytes: Array[Byte], secretKeyBytes: Array[Byte], ivBytes: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance(AES_CBC)
    val key = new SecretKeySpec(secretKeyBytes, AES)
    val iv = new IvParameterSpec(ivBytes)
    cipher.init(Cipher.ENCRYPT_MODE, key, iv)
    cipher.doFinal(messageBytes)
  }

  private def decryptAES(messageBytes: Array[Byte], secretKeyBytes: Array[Byte], ivBytes: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance(AES_CBC)
    val key = new SecretKeySpec(secretKeyBytes, AES)
    val iv = new IvParameterSpec(ivBytes)
    cipher.init(Cipher.DECRYPT_MODE, key, iv)
    cipher.doFinal(messageBytes)
  }
}
