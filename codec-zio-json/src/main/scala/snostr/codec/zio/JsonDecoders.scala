package snostr.codec.zio

import snostr.core.Crypto.EncryptedContent
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

  implicit val encryptedContentDecoder: JsonDecoder[EncryptedContent] = JsonDecoder[Json.Obj].mapOrFail { obj =>
    val fields: Map[String, Json] = obj.fields.toMap

    def field(n: String): Either[String, Json] = fields.get(n) match {
      case Some(json) => Right(json)
      case None => Left(s"$n is missing")
    }

    for {
      version <- field("version").flatMap(_.as[Int])
      nonce <- field("nonce").flatMap(_.as[String])
      ciphertext <- field("ciphertext").flatMap(_.as[String])
    } yield EncryptedContent(
      version = version,
      nonceBase64 = nonce,
      ciphertextBase64 = ciphertext)
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
          SetMetadata(metadata = map, parsedContent = Some(custom.content), parsedTags = custom.tags)
        }
      case NostrEventKindCodes.TextNote => Right(TextNote(custom.content, custom.tags))
      case NostrEventKindCodes.RecommendServer => Right(RecommendServer(custom.content, custom.tags))
      case NostrEventKindCodes.ContactList =>
        val z = Vector.empty[ContactList.Contact]
        val contacts = custom.tags.foldLeft(z) { (acc, x) =>
          x match {
            case p: PTag => (p.recommendedRelayUrl, p.petname) match {
              case (Some(relay), Some(petname)) => acc :+ ContactList.Contact(p.pubkey, relay, petname)
              case (Some(relay), None) => acc :+ ContactList.Contact(p.pubkey, relay, "")
              case _ => acc
            }
            case _ => acc
          }
        }
        Right(ContactList(contacts, custom.content, parsedTags = custom.tags))
      case NostrEventKindCodes.EncryptedDirectMessage04 =>
        custom.tags.find(_.kind.value == "p") match {
          case Some(ptag: PTag) =>
            Right(EncryptedDirectMessage04(
              content = custom.content,
              receiverPublicKey = ptag.pubkey,
              senderPublicKey = senderPubkey,
              parsedTags = custom.tags))
          case _ => Left("invalid encrypted direct message")
        }
      case NostrEventKindCodes.Deletion =>
        catchAll(Deletion(
          content = custom.content,
          eventIds = custom.tags.collect {
            case etag: ETag => etag.eventId
          },
          parsedTags = custom.tags))
      case NostrEventKindCodes.Repost =>
        catchAll(Repost(
          eventId = custom.tags.reverseIterator.collectFirst {
            case tag: ETag => tag.eventId
          }.get,
          relay = custom.tags.reverseIterator.collectFirst {
            case tag: ETag => tag.recommendedRelayUrl
          }.flatten.get,
          authorPublicKey = custom.tags.reverseIterator.collectFirst {
            case tag: PTag => tag.pubkey
          }.get,
          parsedContent = Some(custom.content),
          parsedTags = custom.tags
        ))
      case NostrEventKindCodes.Reaction =>
        catchAll(Reaction(
          content = custom.content,
          eventId = custom.tags.reverseIterator.collectFirst {
            case tag: ETag => tag.eventId
          }.get,
          author = custom.tags.reverseIterator.collectFirst {
            case tag: PTag => tag.pubkey
          }.get,
          parsedTags = custom.tags
        ))
      case NostrEventKindCodes.EncryptedDirectMessage44 =>
        custom.tags.find(_.kind.value == "p") match {
          case Some(ptag: PTag) =>
            Right(EncryptedDirectMessage44(
              content = custom.content,
              receiverPublicKey = ptag.pubkey,
              senderPublicKey = senderPubkey,
              parsedTags = custom.tags))
          case _ => Left("invalid encrypted direct message")
        }
      case NostrEventKindCodes.GiftWrap =>
        custom.tags.find(_.kind.value == "p") match {
          case Some(ptag: PTag) =>
            for {
              content <- JsonDecoder[EncryptedContent].decodeJson(custom.content)
            } yield GiftWrap(
              wrappedContent = content,
              receiverPublicKey = ptag.pubkey,
              senderPublicKey = senderPubkey,
              parsedContent = Some(custom.content),
              parsedTags = custom.tags)
          case _ => Left("invalid encrypted direct message")
        }
      case NostrEventKindCodes.Auth =>
        catchAll(Auth(
          challenge = custom.tags.reverseIterator.collectFirst {
            case tag: ChallengeTag => tag.challenge
          }.get,
          relay = custom.tags.reverseIterator.collectFirst {
            case tag: RelayTag => tag.relay
          }.get,
          parsedContent = Some(custom.content),
          parsedTags = custom.tags
        ))
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

  implicit val authClientMessageDecoder: JsonDecoder[AuthClientMessage] = JsonDecoder[(String, NostrEvent)].mapOrFail { pair =>
    for {
      _ <- checkMessageType(pair._1, NostrClientMessageKinds.AUTH)
    } yield AuthClientMessage(pair._2)
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

  implicit val countClientMessageDecoder: JsonDecoder[CountClientMessage] = JsonDecoder[Json.Arr].mapOrFail { arr =>
    def parseFilters(json: Json.Arr): Either[String, Vector[NostrFilter]] = {
      json.as[Vector[NostrFilter]]
    }

    for {
      arr <- if (arr.elements.size < 3) Left(s"invalid ${NostrClientMessageKinds.COUNT} message") else Right(arr)
      messageType <- head(arr)
      _ <- checkMessageType(messageType, NostrClientMessageKinds.COUNT)
      arr <- tail(arr)
      subscriptionId <- head(arr)
      arr <- tail(arr)
      filters <- parseFilters(arr)
    } yield CountClientMessage(
      subscriptionId = subscriptionId,
      filters = filters)
  }

  implicit val countRelayMessageDecoder: JsonDecoder[CountRelayMessage] = JsonDecoder[(String, String, Map[String, Int])].mapOrFail { triplet =>
    for {
      _ <- checkMessageType(triplet._1, NostrRelayMessageKinds.COUNT)
      count <- triplet._3.get("count").toRight("key not found: count")
    } yield CountRelayMessage(triplet._2, count)
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

  implicit val authRelayMessageDecoder: JsonDecoder[AuthRelayMessage] = JsonDecoder[(String, String)].mapOrFail { pair =>
    for {
      _ <- checkMessageType(pair._1, NostrRelayMessageKinds.AUTH)
    } yield AuthRelayMessage(pair._2)
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
