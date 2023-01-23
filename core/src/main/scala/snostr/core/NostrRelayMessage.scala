package snostr.core

trait NostrRelayMessage {
  val kind: String
}

object NostrRelayMessageKinds {
  val EVENT = "EVENT"
  val NOTICE = "NOTICE"
  val EOSE = "EOSE"
  val OK = "OK"
}

case class EventRelayMessage(subscriptionId: String, event: NostrEvent) extends NostrRelayMessage {
  override val kind: String = NostrRelayMessageKinds.EVENT
}

case class NoticeRelayMessage(message: String) extends NostrRelayMessage {
  override val kind: String = NostrRelayMessageKinds.NOTICE
}

case class EndOfStoredEventsRelayMessage(subscriptionId: String) extends NostrRelayMessage {
  override val kind: String = NostrRelayMessageKinds.EOSE
}

case class OkRelayMessage(eventId: Sha256Digest, saved: Boolean, message: String) extends NostrRelayMessage {
  override val kind: String = NostrRelayMessageKinds.OK
}
