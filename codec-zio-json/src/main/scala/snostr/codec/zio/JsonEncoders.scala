package snostr.codec.zio

import snostr.core._
import zio.Chunk.AnyRefArray
import zio.json.ast.Json
import zio.json.{EncoderOps, JsonEncoder}

object JsonEncoders {
  implicit val sha256DigestEncoder: JsonEncoder[Sha256Digest] = JsonEncoder[String].contramap(_.toHex)

  implicit val nostrPublicKeyEncoder: JsonEncoder[NostrPublicKey] = JsonEncoder[String].contramap(_.toHex)

  implicit val nostrSignatureEncoder: JsonEncoder[NostrSignature] = JsonEncoder[String].contramap(_.toHex)

  implicit val nostrTagEncoder: JsonEncoder[NostrTag] = JsonEncoder[Json.Arr].contramap { tag =>
    val vec = tag.toStrings.map(Json.Str(_))
    Json.Arr(AnyRefArray(vec.toArray, 0, vec.length))
  }

  implicit val nostrEventEncoder: JsonEncoder[NostrEvent] = JsonEncoder[Json].contramap { event =>
    (for {
      tags <- event.kind.tags.toJsonAST
    } yield {
      Json.Obj(
        "id" -> Json.Str(event.id.toHex),
        "pubkey" -> Json.Str(event.pubkey.toHex),
        "created_at" -> Json.Num(event.createdAt.getEpochSecond),
        "kind" -> Json.Num(event.kind.value),
        "tags" -> tags,
        "content" -> Json.Str(event.kind.content),
        "sig" -> Json.Str(event.sig.toHex))
    }) match {
      case Right(value) => value
      case Left(err) => throw new RuntimeException(err)
    }
  }

  implicit val nostrEventCommitmentEncoder: JsonEncoder[NostrEvent.Commitment] = JsonEncoder[Vector[Json]].contramap { commitment =>
    (for {
      tags <- commitment._5.toJsonAST
    } yield {
      Vector(
        Json.Num(commitment._1),
        Json.Str(commitment._2.toHex),
        Json.Num(commitment._3),
        Json.Num(commitment._4),
        tags,
        Json.Str(commitment._6)
      )
    }) match {
      case Right(value) => value
      case Left(err) => throw new RuntimeException(err)
    }
  }

  implicit val nostrFilterEncoder: JsonEncoder[NostrFilter] = JsonEncoder[Json].contramap { filter =>
    def toJsonArr[A](vec: Vector[A])(f: A => Json): Json.Arr = {
      val jstrings = vec.map(f(_))
      Json.Arr(AnyRefArray(jstrings.toArray, 0, jstrings.size))
    }

    def field[A](name: String, vec: Vector[A])(f: A => Json): Option[(String, Json.Arr)] =
      if (vec.isEmpty) {
        None
      } else {
        Some((name, toJsonArr(vec)(f)))
      }

    val tags: Vector[Option[(String, Json.Arr)]] = filter.tags.toVector
      .map { case (k, v) => (s"#${k.value}", v) }
      .sortBy(_._1)
      .map(x => field(x._1, x._2)(s => Json.Str(s)))

    val fields: Vector[(String, Json)] =
      (Vector(
        field("ids", filter.ids)(s => Json.Str(s)),
        field("authors", filter.authors)(s => Json.Str(s)),
        field("kinds", filter.kinds)(s => Json.Num(s))) ++
        tags ++
        Vector(filter.since.map(x => ("since", Json.Num(x))),
          filter.until.map(x => ("until", Json.Num(x))),
          filter.limit.map(x => ("limit", Json.Num(x))),
        )).flatten

    Json.Obj(AnyRefArray(fields.toArray, 0, fields.size))
  }

  implicit val eventClientMessageEncoder: JsonEncoder[EventClientMessage] = JsonEncoder[(String, NostrEvent)]
    .contramap(msg => (msg.kind, msg.event))

  implicit val reqClientMessageEncoder: JsonEncoder[ReqClientMessage] = JsonEncoder[Json.Arr].contramap { msg =>
    val filters = msg.filters.map(_.toJsonAST).collect { case Right(v) => v }
    val vec: Vector[Json] = Vector(Json.Str(msg.kind), Json.Str(msg.subscriptionId)) ++ filters
    Json.Arr(AnyRefArray(vec.toArray, 0, vec.size))
  }

  implicit val closeClientMessageEncoder: JsonEncoder[CloseClientMessage] = JsonEncoder[(String, String)]
    .contramap(msg => (msg.kind, msg.subscriptionId))


  implicit val eventRelayMessageEncoder: JsonEncoder[EventRelayMessage] = JsonEncoder[(String, String, NostrEvent)]
    .contramap(msg => (msg.kind, msg.subscriptionId, msg.event))

  implicit val noticeRelayMessageEncoder: JsonEncoder[NoticeRelayMessage] = JsonEncoder[(String, String)]
    .contramap(msg => (msg.kind, msg.message))

  implicit val endOfStoredEventsNoticeRelayMessageEncoder: JsonEncoder[EndOfStoredEventsRelayMessage] = JsonEncoder[(String, String)]
    .contramap(msg => (msg.kind, msg.subscriptionId))

  implicit val okRelayMessageEncoder: JsonEncoder[OkRelayMessage] = JsonEncoder[(String, Sha256Digest, Boolean, String)]
    .contramap { ok =>
      val message = OkRelayMessage.Result.prefixedMessage(ok.result)
      (ok.kind, ok.eventId, ok.result.saved, message)
    }

  implicit val nostrRelayInformationEncoder: JsonEncoder[NostrRelayInformation] = JsonEncoder[Json].contramap { ri =>
    def toJsonArr[A](vec: Vector[A])(f: A => Json): Json.Arr = {
      val jstrings = vec.map(f(_))
      Json.Arr(AnyRefArray(jstrings.toArray, 0, jstrings.size))
    }

    def fieldVec[A](name: String, vec: Vector[A])(f: A => Json): Option[(String, Json.Arr)] =
      if (vec.isEmpty) {
        None
      } else {
        Some((name, toJsonArr(vec)(f)))
      }

    def field[A](name: String, opt: Option[A])(f: A => Json.Str): Option[(String, Json.Str)] =
      opt.map(x => (name, f(x)))

    val fields: Vector[(String, Json)] =
      Vector(
        field("id", ri.id)(s => Json.Str(s)),
        field("name", ri.name)(s => Json.Str(s)),
        field("description", ri.description)(s => Json.Str(s)),
        field("pubkey", ri.pubkey)(s => Json.Str(s.toHex)),
        field("contact", ri.contact)(s => Json.Str(s)),
        fieldVec("supported_nips", ri.supportedNips)(s => Json.Num(s)),
        field("software", ri.software)(s => Json.Str(s)),
        field("version", ri.version)(s => Json.Str(s)),
        ).flatten

    Json.Obj(AnyRefArray(fields.toArray, 0, fields.size))
  }

}
