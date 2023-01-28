package snostr.core

case class NostrRelayInformation(id: Option[String] = None,
                                 name: Option[String] = None,
                                 description: Option[String] = None,
                                 pubkey: Option[NostrPublicKey] = None,
                                 contact: Option[String] = None,
                                 supportedNips: Vector[Int] = Vector.empty,
                                 software: Option[String] = None,
                                 version: Option[String] = None) {
  private lazy val supported = supportedNips.toSet

  def supported(nips: Int*): Boolean =
    if (supportedNips.isEmpty || nips.isEmpty)
      false
    else
      nips.forall(supported)
}
