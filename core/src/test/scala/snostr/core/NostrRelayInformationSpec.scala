package snostr.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NostrRelayInformationSpec extends AnyFlatSpec with Matchers {

  it should "check supported NIPs" in {
    val empty = NostrRelayInformation()
    empty.supported() should be(false)
    empty.supported(1) should be(false)
    empty.supported(1, 2) should be(false)
    empty.supported(1, 2, 3) should be(false)

    val nonEmpty = NostrRelayInformation(supportedNips = Vector(1, 2, 4, 9, 11, 12, 15, 16, 20, 22, 26, 28, 33))
    nonEmpty.supported() should be(false)
    nonEmpty.supported(3) should be(false)
    nonEmpty.supported(3, 5) should be(false)
    nonEmpty.supported(3, 5, 7) should be(false)
    nonEmpty.supported(40, 3, 5, 7) should be(false)

    nonEmpty.supported(12) should be(true)
    nonEmpty.supported(12, 2) should be(true)
    nonEmpty.supported(12, 2, 33) should be(true)
    nonEmpty.supported(12, 2, 33, 9) should be(true)
  }

}
