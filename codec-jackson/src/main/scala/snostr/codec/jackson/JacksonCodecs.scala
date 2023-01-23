package snostr.codec.jackson

import snostr.core._

import scala.language.implicitConversions

object JacksonCodecs extends Codecs {

  implicit def encodeCommitment(commitment: NostrEvent.Commitment): String =
    JsonSerializers.commitmentToJson(commitment)

  implicit def encodeClientMessage(message: NostrClientMessage): String =
    JsonSerializers.nostrClientMessageToJson(message)

  implicit def decodeClientMessage(json: String): NostrClientMessage =
    JsonSerializers.jsonToNostrClientMessage(json)

  implicit def encodeRelayMessage(message: NostrRelayMessage): String =
    JsonSerializers.nostrRelayMessageToJson(message)

  implicit def decodeRelayMessage(json: String): NostrRelayMessage =
    JsonSerializers.jsonToNostrRelayMessage(json)

}
