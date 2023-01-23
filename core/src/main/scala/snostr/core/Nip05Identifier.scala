package snostr.core

case class Nip05Identifier(localPart: String, domain: String) {
  override def toString: String = s"${localPart.toLowerCase()}@${domain.toLowerCase()}"
}
