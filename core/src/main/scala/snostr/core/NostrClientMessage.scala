package snostr.core

sealed trait NostrClientMessage {
  val kind: String
}

object NostrClientMessageKinds {
  val EVENT = "EVENT"
  val REQ = "REQ"
  val CLOSE = "CLOSE"
  val AUTH = "AUTH"
  val COUNT = "COUNT"
}

case class EventClientMessage(event: NostrEvent) extends NostrClientMessage {
  override val kind: String = NostrClientMessageKinds.EVENT
}

case class ReqClientMessage(subscriptionId: String, filters: Vector[NostrFilter]) extends NostrClientMessage {
  override val kind: String = NostrClientMessageKinds.REQ
}

case class CloseClientMessage(subscriptionId: String) extends NostrClientMessage {
  override val kind: String = NostrClientMessageKinds.CLOSE
}

case class AuthClientMessage(event: NostrEvent) extends NostrClientMessage {
  override val kind: String = NostrClientMessageKinds.AUTH
}

case class CountClientMessage(subscriptionId: String, filters: Vector[NostrFilter]) extends NostrClientMessage {
  override val kind: String = NostrClientMessageKinds.COUNT
}
