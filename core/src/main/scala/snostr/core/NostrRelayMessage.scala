package snostr.core

trait NostrRelayMessage {
  val kind: String
}

object NostrRelayMessageKinds {
  val EVENT = "EVENT"
  val NOTICE = "NOTICE"
  val EOSE = "EOSE"
  val OK = "OK"
  val AUTH = "AUTH"
  val COUNT = "COUNT"
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

case class OkRelayMessage(eventId: Sha256Digest, result: OkRelayMessage.Result) extends NostrRelayMessage {
  override val kind: String = NostrRelayMessageKinds.OK

  def saved: Boolean = result.saved

  def message: String = result.message
}

object OkRelayMessage {
  sealed trait Result {
    def saved: Boolean

    def message: String
  }

  case class Saved(message: String) extends Result {
    override def saved: Boolean = true

    def duplicate: Boolean = message.startsWith(Result.DUPLICATE)
  }

  object Saved {
    def apply(message: String, duplicate: Boolean): Saved = {
      if (duplicate)
        Saved(Result.prefixedMessage(Result.DUPLICATE, message))
      else
        Saved(message)
    }
  }

  trait Rejected extends Result {
    override def saved: Boolean = false
  }

  case class Blocked(message: String) extends Rejected

  case class Invalid(message: String) extends Rejected

  case class Pow(message: String) extends Rejected

  case class RateLimited(message: String) extends Rejected

  case class Error(message: String) extends Rejected

  case class Other(message: String) extends Rejected

  object Result {
    val DUPLICATE = "duplicate:"
    val BLOCKED = "blocked:"
    val INVALID = "invalid:"
    val POW = "pow:"
    val RATE_LIMITED = "rate-limited:"
    val ERROR = "error:"

    def rejected(message: String): Rejected = {
      if (message.startsWith(BLOCKED)) {
        Blocked(message)
      } else if (message.startsWith(INVALID)) {
        Invalid(message)
      } else if (message.startsWith(POW)) {
        Pow(message)
      } else if (message.startsWith(RATE_LIMITED)) {
        RateLimited(message)
      } else if (message.startsWith(ERROR)) {
        Error(message)
      } else {
        Other(message)
      }
    }

    def prefixedMessage(result: Result): String =
      prefixedMessage(messagePrefix(result), result.message)

    def prefixedMessage(prefix: String, message: String): String = {
      if (message.startsWith(prefix)) {
        message
      } else {
        s"$prefix ${message}"
      }
    }

    def messagePrefix(result: Result): String = result match {
      case saved: Saved => if (saved.duplicate) DUPLICATE else ""
      case _: Blocked => BLOCKED
      case _: Invalid => INVALID
      case _: Pow => POW
      case _: RateLimited => RATE_LIMITED
      case _: Error => ERROR
      case _: Rejected => ""
    }
  }
}

case class AuthRelayMessage(challenge: String) extends NostrRelayMessage {
  override val kind: String = NostrRelayMessageKinds.AUTH
}

case class CountRelayMessage(subscriptionId: String, count: Int) extends NostrRelayMessage {
  override val kind: String = NostrRelayMessageKinds.COUNT
}
