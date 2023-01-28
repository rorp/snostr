package snostr.core

trait Codecs {
  def encodeCommitment(commitment: NostrEvent.Commitment): String

  def encodeClientMessage(message: NostrClientMessage): String

  def decodeClientMessage(json: String): NostrClientMessage

  def encodeRelayMessage(message: NostrRelayMessage): String

  def decodeRelayMessage(json: String): NostrRelayMessage

  def encodeRelayInfo(info: NostrRelayInformation): String

  def decodeRelayInfo(json: String): NostrRelayInformation
}

object Codecs {
  // copy&pasted from https://github.com/json4s/json4s/blob/master/ast/shared/src/main/scala/org/json4s/ParserUtil.scala
  def quote(s: String, alwaysEscapeUnicode: Boolean = false): String =
    quote(
      s = s,
      appender = new java.lang.StringBuilder,
      alwaysEscapeUnicode = alwaysEscapeUnicode
    ).toString

  private[core] def quote[T <: java.lang.Appendable](s: String, appender: T, alwaysEscapeUnicode: Boolean): T = { // hot path
    var i = 0
    val l = s.length
    while (i < l) {
      (s(i): @annotation.switch) match {
        case '"' => appender.append("\\\"")
        case '\\' => appender.append("\\\\")
        case '\b' => appender.append("\\b")
        case '\f' => appender.append("\\f")
        case '\n' => appender.append("\\n")
        case '\r' => appender.append("\\r")
        case '\t' => appender.append("\\t")
        case c =>
          val shouldEscape = if (alwaysEscapeUnicode) {
            c >= 0x80
          } else {
            (c >= '\u0000' && c <= '\u001f') || (c >= '\u0080' && c < '\u00a0') || (c >= '\u2000' && c < '\u2100')
          }
          if (shouldEscape)
            appender.append("\\u%04X".format(c: Int))
          else appender.append(c)
      }
      i += 1
    }
    appender
  }
}
