package snostr.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import snostr.core.TestUtil.testVectors

import scala.util.{Failure, Success, Try}

class NostrSignatureSpec extends AnyFlatSpec with Matchers {

  it should "process the test vectors" in {

    testVectors().collect { case (_, Some(secretKey), _, auxrand, message, signature, _, _) =>
      NostrSignature.sign(message, secretKey, auxrand.get) should be(signature)
    }.size should be(4)

    val results = testVectors().map { case (_, _, publicKey, _, message, signature, result, _) =>
      Try(signature.verify(message, publicKey)) match {
        case Success(actualResult) =>
          actualResult should be(result)
          Some(())
        case Failure(_) =>
          false should be(result)
          None
      }
    }
    results.size should be(15)
    results.count(_.isEmpty) should be(2)
  }

  it should "sign with a random auxrand" in {
    val (_, Some(secretKey), _, _, message, _, _, _) = testVectors().head

    val sig1 = NostrSignature.sign(message, secretKey)
    val sig2 = NostrSignature.sign(message, secretKey)

    sig1 shouldNot be(sig2)

    val publicKey = secretKey.publicKey

    sig1.verify(message, publicKey) should be(true)
    sig2.verify(message, publicKey) should be(true)
  }

}
