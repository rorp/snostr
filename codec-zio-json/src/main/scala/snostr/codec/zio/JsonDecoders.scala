package snostr.codec.zio

import snostr.core._
import zio.Chunk
import zio.json.JsonDecoder
import zio.json.ast.{Json, JsonCursor}

import java.time.Instant
import scala.annotation.tailrec
import scala.util.{Failure, Right, Success, Try}

object JsonDecoders {
  implicit val sha256DigestDecoder: JsonDecoder[Sha256Digest] = JsonDecoder[String].mapOrFail(hex =>
    catchAll(Sha256Digest.fromHex(hex))
  )

  implicit val nostrPublicKeyDecoder: JsonDecoder[NostrPublicKey] = JsonDecoder[String].mapOrFail(hex =>
    catchAll(NostrPublicKey.fromHex(hex))
  )

  implicit val nostrSignatureDecoder: JsonDecoder[NostrSignature] = JsonDecoder[String].mapOrFail(hex =>
    catchAll(NostrSignature.fromHex(hex))
  )

  implicit val nostrTagDecoder: JsonDecoder[NostrTag] = JsonDecoder[Json.Arr].mapOrFail { arr =>
    @tailrec
    def toStringVector(chunk: Chunk[Json], acc: Vector[String]): Either[String, Vector[String]] = {
      if (chunk.isEmpty) {
        Right(acc)
      } else {
        chunk.head.as[String] match {
          case Left(err) => Left(err)
          case Right(value) =>
            toStringVector(chunk.tail, acc :+ value)
        }
      }
    }

    for {
      vec <- toStringVector(arr.elements, Vector())
      tag <- catchAll(NostrTag.fromStrings(vec))
    } yield tag
  }

  implicit val nostrEventDecoder: JsonDecoder[NostrEvent] = JsonDecoder[Json.Obj].mapOrFail { obj =>
    val fields: Map[String, Json] = obj.fields.toMap

    def field(n: String): Either[String, Json] = fields.get(n) match {
      case Some(json) => Right(json)
      case None => Left(s"$n is missing")
    }

    def parseTyped(custom: Custom, senderPubkey: NostrPublicKey): Either[String, NostrEventKind] = custom.value match {
      case NostrEventKindCodes.SetMetadata =>
        for {
          map <- JsonDecoder[Map[String, String]].decodeJson(custom.content)
        } yield {
          SetMetadata(map, Some(custom.content), custom.tags)
        }
      case NostrEventKindCodes.TextNote => Right(TextNote(custom.content, custom.tags))
      case NostrEventKindCodes.RecommendServer => Right(RecommendServer(custom.content, custom.tags))
      case NostrEventKindCodes.ContactList =>
        val z = (Vector.empty[ContactList.Contact], Vector.empty[NostrTag])
        val (contacts, extraTags) = custom.tags.foldLeft(z) { (acc, x) =>
          val (contacts, extraTags) = acc
          x match {
            case p: PTag => (p.recommendedRelayUrl, p.petname) match {
              case (Some(relay), Some(petname)) => (contacts :+ ContactList.Contact(p.pubkey, relay, petname), extraTags)
              case (Some(relay), None) => (contacts :+ ContactList.Contact(p.pubkey, relay, ""), extraTags)
              case _ => (contacts, extraTags :+ p)
            }
            case tag => (contacts, extraTags :+ tag)
          }
        }
        Right(ContactList(contacts, custom.content, extraTags))
      case NostrEventKindCodes.EncryptedDirectMessage =>
        custom.tags.find(_.kind.value == "p") match {
          case Some(ptag: PTag) =>
            val extraTags = custom.tags.filterNot(_ == ptag)
            Right(EncryptedDirectMessage(
              content = custom.content,
              receiverPublicKey = ptag.pubkey,
              senderPublicKey = senderPubkey,
              extraTags = extraTags))
          case _ => Left("invalid encrypted direct message")
        }
      case NostrEventKindCodes.Deletion =>
        catchAll(Deletion(
          content = custom.content,
          eventIds = custom.tags.collect {
            case etag: ETag => etag.eventId
          }))
      case _ => Right(custom)
    }

    val genericEventKind = for {
      kind <- field("kind").flatMap(_.as[Int])
      tags <- field("tags").flatMap(_.as[Vector[NostrTag]])
      content <- field("content").flatMap(_.as[String])
    } yield Custom(kind, content, tags)

    for {
      custom <- genericEventKind
      id <- field("id").flatMap(_.as[Sha256Digest])
      pubkey <- field("pubkey").flatMap(_.as[NostrPublicKey])
      kind <- parseTyped(custom, pubkey)
      createdAt <- field("created_at").flatMap(_.as[Long]).map(Instant.ofEpochSecond)
      sig <- field("sig").flatMap(_.as[NostrSignature])
    } yield {
      NostrEvent(id, pubkey, createdAt, sig, kind)
    }
  }

  implicit val nostrFilterDecoder: JsonDecoder[NostrFilter] = JsonDecoder[Json.Obj].mapOrFail { obj =>
    val fields: Map[String, Json] = obj.fields.toMap

    def field(n: String): Option[Json] = fields.get(n)

    def toLong(n: String): Either[String, Option[Long]] = field(n) match {
      case None => Right(None)
      case Some(json) => json.as[Long].map(Some(_))
    }

    def toVector[A](n: String)(f: Json => Either[String, Vector[A]]): Either[String, Vector[A]] = {
      field(n) match {
        case None => Right(Vector.empty[A])
        case Some(json) => f(json)
      }
    }

    def toStringVector(n: String): Either[String, Vector[String]] = toVector(n)(j => j.as[Vector[String]])

    def toIntVector(n: String): Either[String, Vector[Int]] = toVector(n)(j => j.as[Vector[Int]])

    def parseTags: Either[String, Map[NostrTagKind, Vector[String]]] = {
      val tagFields: Vector[(String, Json)] = fields
        .filter(_._1.startsWith("#"))
        .map { case (k, v) => (k.drop(1), v) }
        .toVector

      val z: Either[String, Map[NostrTagKind, Vector[String]]] = Right(Map.empty)

      tagFields.foldLeft(z) { (acc, field) =>
        val (k, v) = field
        for {
          map <- acc
          vec <- v.as[Vector[String]]
        } yield map.updated(NostrTagKind.fromString(k), vec)
      }
    }

    for {
      ids <- toStringVector("ids")
      authors <- toStringVector("authors")
      kinds <- toIntVector("kinds")
      since <- toLong("since")
      until <- toLong("until")
      limit <- toLong("limit")
      tags <- parseTags
    } yield NostrFilter(
      ids = ids,
      authors = authors,
      kinds = kinds,
      tags = tags,
      since = since,
      until = until,
      limit = limit.map(_.toInt)
    )
  }

  implicit val eventClientMessageDecoder: JsonDecoder[EventClientMessage] = JsonDecoder[(String, NostrEvent)].mapOrFail { pair =>
    for {
      _ <- checkMessageType(pair._1, NostrClientMessageKinds.EVENT)
    } yield EventClientMessage(pair._2)
  }

  implicit val reqClientMessageDecoder: JsonDecoder[ReqClientMessage] = JsonDecoder[Json.Arr].mapOrFail { arr =>
    def parseFilters(json: Json.Arr): Either[String, Vector[NostrFilter]] = {
      json.as[Vector[NostrFilter]]
    }

    for {
      arr <- if (arr.elements.size < 3) Left(s"invalid ${NostrClientMessageKinds.REQ} message") else Right(arr)
      messageType <- head(arr)
      _ <- checkMessageType(messageType, NostrClientMessageKinds.REQ)
      arr <- tail(arr)
      subscriptionId <- head(arr)
      arr <- tail(arr)
      filters <- parseFilters(arr)
    } yield ReqClientMessage(
      subscriptionId = subscriptionId,
      filters = filters)
  }

  implicit val closeClientMessageDecoder: JsonDecoder[CloseClientMessage] = JsonDecoder[(String, String)].mapOrFail { pair =>
    for {
      _ <- checkMessageType(pair._1, NostrClientMessageKinds.CLOSE)
    } yield CloseClientMessage(pair._2)
  }

  implicit val eventRelayMessageDecoder: JsonDecoder[EventRelayMessage] = JsonDecoder[(String, String, NostrEvent)].mapOrFail { triplet =>
    for {
      _ <- checkMessageType(triplet._1, NostrRelayMessageKinds.EVENT)
    } yield EventRelayMessage(triplet._2, triplet._3)
  }

  implicit val noticeRelayMessageDecoder: JsonDecoder[NoticeRelayMessage] = JsonDecoder[(String, String)].mapOrFail { pair =>
    for {
      _ <- checkMessageType(pair._1, NostrRelayMessageKinds.NOTICE)
    } yield NoticeRelayMessage(pair._2)
  }

  implicit val endOfStoredEventsNoticeRelayMessageDecoder: JsonDecoder[EndOfStoredEventsRelayMessage] = JsonDecoder[(String, String)].mapOrFail { pair =>
    for {
      _ <- checkMessageType(pair._1, NostrRelayMessageKinds.EOSE)
    } yield EndOfStoredEventsRelayMessage(pair._2)
  }

  implicit val okRelayMessageDecoder: JsonDecoder[OkRelayMessage] = JsonDecoder[(String, Sha256Digest, Boolean, String)].mapOrFail { tuple =>
    for {
      _ <- checkMessageType(tuple._1, NostrRelayMessageKinds.OK)
    } yield {
      val result = if (tuple._3) {
        OkRelayMessage.Saved(tuple._4)
      } else {
        OkRelayMessage.Result.rejected(tuple._4)
      }
      OkRelayMessage(tuple._2, result)
    }
  }

  implicit val nostrRelayInformationDecoder: JsonDecoder[NostrRelayInformation] = JsonDecoder[Json.Obj].mapOrFail { obj =>
    val fields: Map[String, Json] = obj.fields.toMap

    def field(n: String): Option[Json] = fields.get(n)

    def toLong(n: String): Either[String, Option[Long]] = field(n) match {
      case None => Right(None)
      case Some(json) => json.as[Long].map(Some(_))
    }

    def toString(n: String): Either[String, Option[String]] = field(n) match {
      case None => Right(None)
      case Some(json) => json.as[String].map(Some(_))
    }

    def toVector[A](n: String)(f: Json => Either[String, Vector[A]]): Either[String, Vector[A]] = {
      field(n) match {
        case None => Right(Vector.empty[A])
        case Some(json) => f(json)
      }
    }

    def toIntVector(n: String): Either[String, Vector[Int]] = toVector(n)(j => j.as[Vector[Int]])

    for {
      id <- toString("id")
      name <- toString("name")
      description <- toString("description")
      pubkey <- toString("pubkey").map(opt => opt.flatMap(s => Try(NostrPublicKey.fromHex(s)).toOption))
      contact <- toString("contact")
      supportedNips <- toIntVector("supported_nips")
      software <- toString("software")
      version <- toString("version")
    } yield NostrRelayInformation(
      id, name, description, pubkey, contact, supportedNips, software, version
    )
  }


  private def head(arr: Json.Arr): Either[String, String] = arr.get(JsonCursor.element(0)).flatMap(_.as[String])

  private def tail(arr: Json.Arr): Right[String, Json.Arr] = Right(Json.Arr(arr.elements.tail))

  private def checkMessageType(msgType: String, expectedMessageType: String): Either[String, String] = {
    if (msgType == expectedMessageType) {
      Right(msgType)
    } else {
      Left(s"unexpected message type: `$msgType`")
    }
  }

  private def catchAll[A](t: => A): Either[String, A] = Try(t) match {
    case Success(value) => Right(value)
    case Failure(ex) => Left(ex.getMessage)
  }
}
