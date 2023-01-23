package snostr.core

sealed trait NostrClientMessage {
  val kind: String
}

object NostrClientMessageKinds {
  val EVENT = "EVENT"
  val REQ = "REQ"
  val CLOSE = "CLOSE"
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
