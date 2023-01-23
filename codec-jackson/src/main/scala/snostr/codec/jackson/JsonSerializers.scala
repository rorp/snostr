package snostr.codec.jackson

import org.json4s._
import org.json4s.jackson.Serialization
import snostr.core._

import java.time.Instant

object JsonSerializers {
  implicit val serialization: Serialization.type = jackson.Serialization

  implicit val formats: Formats = org.json4s.DefaultFormats +
    Sha256DigestSerializer +
    NostrPublicKeySerializer +
    NostrSignatureSerializer +
    NostrTagSerializer +
    NostrEventSerializer +
    NostrFilterSerializer +
    CloseClientMessageSerializer +
    EventClientMessageSerializer +
    EventRelayMessageSerializer +
    NoticeRelayMessageSerializer +
    EndOfStoredEventsRelayMessageSerializer +
    OkRelayMessageSerializer +
    ReqClientMessageSerializer

  val clientFormats: Formats = formats + NostrClientMessageSerializer

  val relayFormats: Formats = formats + NostrRelayMessageSerializer

  def commitmentToJson(commitment: NostrEvent.Commitment): String = {
    val vec = JArray(List(
      Extraction.decompose(commitment._1),
      Extraction.decompose(commitment._2),
      Extraction.decompose(commitment._3),
      Extraction.decompose(commitment._4),
      Extraction.decompose(commitment._5),
      Extraction.decompose(commitment._6),
    ))
    Serialization.write(vec)
  }

  def nostrClientMessageToJson(message: NostrClientMessage): String =
    serialization.write(message)

  def jsonToNostrClientMessage(json: String): NostrClientMessage =
    serialization.read[NostrClientMessage](json)(clientFormats, manifest[NostrClientMessage])

  def nostrRelayMessageToJson(message: NostrRelayMessage): String = serialization.write(message)

  def jsonToNostrRelayMessage(json: String): NostrRelayMessage =
    serialization.read[NostrRelayMessage](json)(relayFormats, manifest[NostrRelayMessage])

  object Sha256DigestSerializer extends CustomSerializer[Sha256Digest](_ => ( {
    case JString(hex) => Sha256Digest.fromHex(hex)
  }, {
    case x: Sha256Digest => JString(x.toHex)
  }))

  object NostrPublicKeySerializer extends CustomSerializer[NostrPublicKey](_ => ( {
    case JString(hex) => NostrPublicKey.fromHex(hex)
  }, {
    case x: NostrPublicKey => JString(x.toHex)
  }))

  object NostrSignatureSerializer extends CustomSerializer[NostrSignature](_ => ( {
    case JString(hex) => NostrSignature.fromHex(hex)
  }, {
    case x: NostrSignature => JString(x.toHex)
  }))

  object NostrTagSerializer extends CustomSerializer[NostrTag](formats => ( {
    case tagArray: JArray =>
      val vec = tagArray.extract[Vector[String]](formats, manifest[Vector[String]])
      NostrTag.fromStrings(vec)
  }, {
    case tag: NostrTag =>
      val list = tag.toStrings.map(JString(_)).toList
      JArray(list)
  }))

  object NostrEventSerializer extends CustomSerializer[NostrEvent](_ => ( {
    case JObject(list) =>
      val fields = list.toMap

      def field[A](n: String, mf: scala.reflect.Manifest[A]): A =
        fields
          .getOrElse(n, throw new MappingException(s"${n} is missing"))
          .extract(formats, mf)

      val id = field("id", manifest[Sha256Digest])
      val pubkey = field("pubkey", manifest[NostrPublicKey])
      val createdAt = Instant.ofEpochSecond(field("created_at", manifest[Long]))
      val custom = Custom(
        value = field("kind", manifest[Int]),
        content = field("content", manifest[String]),
        tags = field("tags", manifest[Vector[NostrTag]])
      )
      val sig = field("sig", manifest[NostrSignature])

      val kind = custom.value match {
        case NostrEventKindCodes.SetMetadata =>
          val map = serialization.read(custom.content)(formats, manifest[Map[String, String]])
          SetMetadata(map, Some(custom.content), custom.tags)
        case NostrEventKindCodes.TextNote => TextNote(custom.content, custom.tags)
        case NostrEventKindCodes.RecommendServer => RecommendServer(custom.content, custom.tags)
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
          ContactList(contacts, custom.content, extraTags)
        case NostrEventKindCodes.EncryptedDirectMessage =>
          custom.tags.find(_.kind.value == "p") match {
            case Some(ptag: PTag) =>
              val extraTags = custom.tags.filterNot(_ == ptag)
              EncryptedDirectMessage(
                content = custom.content,
                receiverPublicKey = ptag.pubkey,
                senderPublicKey = pubkey,
                extraTags = extraTags)
            case _ => throw new MappingException("invalid encrypted direct message")
          }
        case NostrEventKindCodes.Deletion =>
          Deletion(
            content = custom.content,
            eventIds = custom.tags.collect {
              case etag: ETag => etag.eventId
            })
        case _ => custom
      }

      NostrEvent(id, pubkey, createdAt, sig, kind)
  }, {
    case event: NostrEvent =>
      val tags = Extraction.decompose(event.kind.tags)
      JObject(
        "id" -> JString(event.id.toHex),
        "pubkey" -> JString(event.pubkey.toHex),
        "created_at" -> JInt(event.createdAt.getEpochSecond),
        "kind" -> JInt(event.kind.value),
        "tags" -> tags,
        "content" -> JString(event.kind.content),
        "sig" -> JString(event.sig.toHex))
  }))

  object NostrFilterSerializer extends CustomSerializer[NostrFilter](formats => ( {
    case obj: JObject =>
      val fields = obj.obj.toMap

      def field[A](n: String, mf: scala.reflect.Manifest[A]): Option[A] =
        fields.get(n).map(_.extract(formats, mf))

      val tags: Map[NostrTagKind, Vector[String]] = fields
        .filter(_._1.startsWith("#"))
        .map { case (k, v) => (NostrTagKind.fromString(k.drop(1)), v.extract(formats, manifest[Vector[String]])) }

      NostrFilter(
        ids = field("ids", manifest[Vector[String]]).getOrElse(Vector.empty),
        authors = field("authors", manifest[Vector[String]]).getOrElse(Vector.empty),
        kinds = field("kinds", manifest[Vector[Int]]).getOrElse(Vector.empty),
        tags = tags,
        since = field("since", manifest[Long]),
        until = field("until", manifest[Long]),
        limit = field("limit", manifest[Int])
      )
  }, {
    case filter: NostrFilter =>
      def toJArray[A](n: String, vec: Vector[A]): Option[(String, JArray)] =
        if (vec.isEmpty) None else
          Some((n, JArray(vec.map(x => Extraction.decompose(x)(formats)).toList)))

      val fileds = List(
        toJArray("ids", filter.ids),
        toJArray("authors", filter.authors),
        toJArray("kinds", filter.kinds),
      ) ++
        filter.tags
          .map { case (k, v) => (s"#${k.value}", Extraction.decompose(v)(formats)) }
          .toList
          .sortBy(_._1)
          .map(Some(_)) ++
        List(
          filter.since.map(x => ("since", Extraction.decompose(x)(formats))),
          filter.until.map(x => ("until", Extraction.decompose(x)(formats))),
          filter.limit.map(x => ("limit", Extraction.decompose(x)(formats)))
        )

      JObject(fileds.flatten)
  })) {
  }

  object CloseClientMessageSerializer extends CustomSerializer[CloseClientMessage](formats => ( {
    case arr: JArray => readCloseClientMessage(arr, formats)
  }, {
    case msg: CloseClientMessage => JArray(List(JString(NostrClientMessageKinds.CLOSE), JString(msg.subscriptionId)))
  })) {
  }

  object EventClientMessageSerializer extends CustomSerializer[EventClientMessage](formats => ( {
    case arr: JArray => readEventClientMessage(arr, formats)
  }, {
    case msg: EventClientMessage =>
      JArray(List(JString(NostrClientMessageKinds.EVENT), Extraction.decompose(msg.event)(formats)))
  }))

  object EventRelayMessageSerializer extends CustomSerializer[EventRelayMessage](formats => ( {
    case arr: JArray => readEventRelayMessage(arr, formats)
      arr.arr match {
        case (kind: JString) :: (subId: JString) :: (obj: JObject) :: Nil =>
          checkMessageType(kind, NostrRelayMessageKinds.EVENT)
          EventRelayMessage(subId.s, Extraction.extract(obj)(formats, manifest[NostrEvent]))
        case _ => throw new MappingException(s"invalid ${NostrRelayMessageKinds.EVENT} message")
      }
  }, {
    case msg: EventRelayMessage =>
      JArray(List(JString(NostrRelayMessageKinds.EVENT), JString(msg.subscriptionId), Extraction.decompose(msg.event)(formats)))
  }))

  object NoticeRelayMessageSerializer extends CustomSerializer[NoticeRelayMessage](formats => ( {
    case arr: JArray => readNoticeRelayMessage(arr, formats)
  }, {
    case msg: NoticeRelayMessage => JArray(List(JString(NostrRelayMessageKinds.NOTICE), JString(msg.message)))
  }))

  object EndOfStoredEventsRelayMessageSerializer extends CustomSerializer[EndOfStoredEventsRelayMessage](formats => ( {
    case arr: JArray => readEOSERelayMessage(arr, formats)
  }, {
    case msg: EndOfStoredEventsRelayMessage => JArray(List(JString(NostrRelayMessageKinds.EOSE), JString(msg.subscriptionId)))
  }))

  object OkRelayMessageSerializer extends CustomSerializer[OkRelayMessage](formats => ( {
    case arr: JArray => readOkRelayMessage(arr, formats)
  }, {
    case msg: OkRelayMessage => JArray(List(JString(NostrRelayMessageKinds.OK), JString(msg.eventId.toHex), JBool(msg.saved), JString(msg.message)))
  }))

  object ReqClientMessageSerializer extends CustomSerializer[ReqClientMessage](formats => ( {
    case arr: JArray => readReqClientMessage(arr, formats)
  }, {
    case msg: ReqClientMessage =>
      val filters = msg.filters.map(filter => Extraction.decompose(filter)(formats)).toList
      JArray(JString(NostrClientMessageKinds.REQ) :: JString(msg.subscriptionId) :: filters)
  }))

  object NostrClientMessageSerializer extends CustomSerializer[NostrClientMessage](formats => ( {
    case arr: JArray => arr.arr.headOption.map(_.extract[String](formats, manifest[String])) match {
      case Some(kind) => kind match {
        case NostrClientMessageKinds.CLOSE => readCloseClientMessage(arr, formats)
        case NostrClientMessageKinds.EVENT => readEventClientMessage(arr, formats)
        case NostrClientMessageKinds.REQ => readReqClientMessage(arr, formats)
        case err => throw new MappingException(s"unknown client message `$err`")
      }
      case _ => throw new MappingException("invalid client message")
    }
  }, {
    case msg: CloseClientMessage => Extraction.decompose(msg)(formats)
    case msg: EventClientMessage => Extraction.decompose(msg)(formats)
    case msg: ReqClientMessage => Extraction.decompose(msg)(formats)
  }))

  object NostrRelayMessageSerializer extends CustomSerializer[NostrRelayMessage](formats => ( {
    case arr: JArray => arr.arr.headOption.map(_.extract[String](formats, manifest[String])) match {
      case Some(kind) => kind match {
        case NostrRelayMessageKinds.EVENT => readEventRelayMessage(arr, formats)
        case NostrRelayMessageKinds.NOTICE => readNoticeRelayMessage(arr, formats)
        case NostrRelayMessageKinds.EOSE => readEOSERelayMessage(arr, formats)
        case NostrRelayMessageKinds.OK => readOkRelayMessage(arr, formats)
        case err => throw new MappingException(s"unknown relay message `$err`")
      }
      case _ => throw new MappingException("invalid client message")
    }
  }, {
    case msg: EventRelayMessage => Extraction.decompose(msg)(formats)
    case msg: NoticeRelayMessage => Extraction.decompose(msg)(formats)
    case msg: EndOfStoredEventsRelayMessage => Extraction.decompose(msg)(formats)
    case msg: OkRelayMessage => Extraction.decompose(msg)(formats)
  }))

  private def readCloseClientMessage(arr: JArray, formats: Formats) = {
    arr.arr match {
      case (kind: JString) :: (subId: JString) :: Nil =>
        checkMessageType(kind, NostrClientMessageKinds.CLOSE)
        CloseClientMessage(subId.s)
      case _ => throw new MappingException(s"invalid ${NostrClientMessageKinds.CLOSE} message")
    }
  }

  private def readEventClientMessage(arr: JArray, formats: Formats) = {
    arr.arr match {
      case (kind: JString) :: (obj: JObject) :: Nil =>
        checkMessageType(kind, NostrClientMessageKinds.EVENT)
        EventClientMessage(Extraction.extract(obj)(formats, manifest[NostrEvent]))
      case _ => throw new MappingException(s"invalid ${NostrClientMessageKinds.EVENT} message")
    }
  }

  private def readEventRelayMessage(arr: JArray, formats: Formats) = {
    arr.arr match {
      case (kind: JString) :: (subId: JString) :: (obj: JObject) :: Nil =>
        checkMessageType(kind, NostrRelayMessageKinds.EVENT)
        EventRelayMessage(subId.s, Extraction.extract(obj)(formats, manifest[NostrEvent]))
      case _ => throw new MappingException(s"invalid ${NostrRelayMessageKinds.EVENT} message")
    }
  }

  private def readNoticeRelayMessage(arr: JArray, formats: Formats) = {
    arr.arr match {
      case (kind: JString) :: (msg: JString) :: Nil =>
        checkMessageType(kind, NostrRelayMessageKinds.NOTICE)
        NoticeRelayMessage(msg.s)
      case _ => throw new MappingException(s"invalid ${NostrRelayMessageKinds.NOTICE} message")
    }
  }

  private def readEOSERelayMessage(arr: JArray, formats: Formats) = {
    arr.arr match {
      case (kind: JString) :: (msg: JString) :: Nil =>
        checkMessageType(kind, NostrRelayMessageKinds.EOSE)
        EndOfStoredEventsRelayMessage(msg.s)
      case _ => throw new MappingException(s"invalid ${NostrRelayMessageKinds.EOSE} message")
    }
  }

  private def checkMessageType(msgType: JString, expectedMessageType: String): Unit = {
    if (msgType.s != expectedMessageType) {
      throw new MappingException(s"unexpected message type: `${msgType.s}`")
    }
  }

  private def readOkRelayMessage(arr: JArray, formats: Formats) = {
    arr.arr match {
      case (kind: JString) :: (eventId: JString) :: (saved: JBool) :: (msg: JString) :: Nil =>
        checkMessageType(kind, NostrRelayMessageKinds.OK)
        OkRelayMessage(Sha256Digest.fromHex(eventId.s), saved.value, msg.s)
      case _ => throw new MappingException(s"invalid ${NostrRelayMessageKinds.OK} message")
    }
  }

  private def readReqClientMessage(arr: JArray, formats: Formats) = {
    arr.arr match {
      case (kind: JString) :: (subId: JString) :: filterList =>
        checkMessageType(kind, NostrClientMessageKinds.REQ)
        if (filterList.isEmpty) {
          throw new MappingException(s"invalid ${NostrClientMessageKinds.REQ} message")
        }
        val filters = filterList.map {
          case filter: JObject => Extraction.extract(filter)(formats, manifest[NostrFilter])
          case _ => throw new MappingException("invalid filter")
        }
        ReqClientMessage(subId.s, filters.toVector)
      case err =>
        throw new MappingException(s"invalid ${NostrClientMessageKinds.REQ} message $err")
    }
  }
}