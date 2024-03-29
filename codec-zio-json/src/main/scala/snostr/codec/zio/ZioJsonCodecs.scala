package snostr.codec.zio

import snostr.core._
import zio.json.{EncoderOps, JsonDecoder}

import scala.annotation.tailrec
import scala.language.implicitConversions

object ZioJsonCodecs extends Codecs {

  def encodeCommitment(commitment: NostrEvent.Commitment): String = {
    import JsonEncoders.nostrEventCommitmentEncoder
    commitment.toJson
  }

  def encodeClientMessage(message: NostrClientMessage): String = {
    import JsonEncoders._
    message match {
      case close: CloseClientMessage => close.toJson
      case event: EventClientMessage => event.toJson
      case req: ReqClientMessage => req.toJson
      case auth: AuthClientMessage => auth.toJson
      case req: CountClientMessage => req.toJson
    }
  }

  def decodeClientMessage(json: String): NostrClientMessage = {
    import JsonDecoders._
    chainEithers(json, Seq(
      JsonDecoder[CloseClientMessage].decodeJson,
      JsonDecoder[EventClientMessage].decodeJson,
      JsonDecoder[ReqClientMessage].decodeJson,
      JsonDecoder[AuthClientMessage].decodeJson,
      JsonDecoder[CountClientMessage].decodeJson,
    ))
  }

  def encodeRelayMessage(message: NostrRelayMessage): String = {
    import JsonEncoders._
    message match {
      case notice: NoticeRelayMessage => notice.toJson
      case event: EventRelayMessage => event.toJson
      case eose: EndOfStoredEventsRelayMessage => eose.toJson
      case ok: OkRelayMessage => ok.toJson
      case auth: AuthRelayMessage => auth.toJson
      case count: CountRelayMessage => count.toJson
    }
  }

  def decodeRelayMessage(json: String): NostrRelayMessage = {
    import JsonDecoders._
    chainEithers(json, Seq(
      JsonDecoder[NoticeRelayMessage].decodeJson,
      JsonDecoder[EndOfStoredEventsRelayMessage].decodeJson,
      JsonDecoder[OkRelayMessage].decodeJson,
      JsonDecoder[EventRelayMessage].decodeJson,
      JsonDecoder[AuthRelayMessage].decodeJson,
      JsonDecoder[CountRelayMessage].decodeJson,
    ))
  }

  private def chainEithers[A](s: String, fs: Seq[String => Either[String, A]]): A = {

    @tailrec
    def loop(fs: Seq[String => Either[String, A]]): A = {
      fs.headOption match {
        case None =>
          throw new IllegalArgumentException("empty seq")
        case Some(f) =>
          f(s) match {
            case Right(value) =>
              value
            case Left(err) =>
              if (fs.tail.nonEmpty) {
                loop(fs.tail)
              } else {
                throw new IllegalArgumentException(err)
              }
          }
      }
    }

    loop(fs)
  }

  override def encodeRelayInfo(info: NostrRelayInformation): String = {
    import JsonEncoders._
    info.toJson
  }

  override def decodeRelayInfo(json: String): NostrRelayInformation = {
    import JsonDecoders._
    chainEithers(json, Seq(JsonDecoder[NostrRelayInformation].decodeJson))
  }

  override def encodeNostrEvent(event: NostrEvent): String = {
    import JsonEncoders._
    event.toJson
  }

  override def decodeNostrEvent(json: String): NostrEvent = {
    import JsonDecoders._
    chainEithers(json, Seq(JsonDecoder[NostrEvent].decodeJson))
  }
}
