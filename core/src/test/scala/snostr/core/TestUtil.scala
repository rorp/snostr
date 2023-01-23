package snostr.core

import fr.acinq.bitcoin.{ByteVector, ByteVector32}

import scala.io.Source
import scala.util.Try

object TestUtil {

  /**
   * Reads the Schnorr test vectors from resources
   * See https://raw.githubusercontent.com/bitcoin/bips/master/bip-0340/test-vectors.csv
   */
  def testVectors(): Vector[(Int, Option[NostrPrivateKey], NostrPublicKey, Option[ByteVector32], ByteVector, NostrSignature, Boolean, String)] = {
    Source.fromResource("test-vectors.csv")
      .getLines()
      .toVector
      .tail
      .map { line =>
        val vec = line.split(",")
        (if (vec.size == 7) {
          vec.appended("")
        } else {
          vec
        }) match {
          case Array(index, secretKey, publicKey, auxrand, message, signature, result, comment) =>
            (
              index.toInt,
              Try(NostrPrivateKey.fromHex(secretKey)).toOption,
              NostrPublicKey.fromHex(publicKey),
              Try(new ByteVector32(auxrand)).toOption,
              new ByteVector(message),
              NostrSignature.fromHex(signature),
              result == "TRUE",
              comment
            )

        }
      }
  }

}
