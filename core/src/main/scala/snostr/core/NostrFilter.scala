package snostr.core

case class NostrFilter(ids: Vector[String] = Vector.empty,
                       authors: Vector[String] = Vector.empty,
                       kinds: Vector[Int] = Vector.empty,
                       tags: Map[NostrTagKind, Vector[String]] = Map.empty,
                       since: Option[Long] = None,
                       until: Option[Long] = None,
                       limit: Option[Int] = None) {
  def e: Vector[Sha256Digest] = tags
    .getOrElse(E, Vector.empty)
    .map(s => Sha256Digest.fromHex(s))

  def p: Vector[NostrPublicKey] = tags
    .getOrElse(P, Vector.empty)
    .map(s => NostrPublicKey.fromHex(s))

  def withIds(values: Vector[String]): NostrFilter = copy(ids = values)

  def withAuthors(values: Vector[String]): NostrFilter = copy(authors = values)

  def witKinds(values: Vector[Int]): NostrFilter = copy(kinds = values)

  def withE(values: Vector[Sha256Digest]): NostrFilter = copy(tags = tags.updated(E, values.map(_.toHex)))

  def withP(values: Vector[NostrPublicKey]): NostrFilter = copy(tags = tags.updated(P, values.map(_.toHex)))

  def withTag(kind: NostrTagKind, values: Vector[String]): NostrFilter = copy(tags = tags.updated(kind, values))

  def withSince(value: Long): NostrFilter = copy(since = Some(value))

  def withUntil(value: Long): NostrFilter = copy(until = Some(value))

  def withLimit(values: Int): NostrFilter = copy(limit = Some(values))
}
