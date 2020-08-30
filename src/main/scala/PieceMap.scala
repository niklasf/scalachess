package chess

case class PieceMap private (
  occupied: PosSet,
  colors: Color.Map[PosSet],
  pawn: PosSet,
  knight: PosSet,
  bishop: PosSet,
  rook: PosSet,
  queen: PosSet,
  king: PosSet
) {
  private def color(pos: Pos): Option[Color] =
    if (colors.white.has(pos)) Some(White)
    else if (colors.black.has(pos)) Some(Black)
    else None

  def apply(pos: Pos): Option[Piece] = color(pos) map { color =>
    if (pawn.has(pos)) color.pawn
    else if (knight.has(pos)) color.knight
    else if (bishop.has(pos)) color.bishop
    else if (rook.has(pos)) color.rook
    else if (queen.has(pos)) color.queen
    else color.king
  }

  def apply(color: Color): PosSet = colors(color)

  def apply(role: Role): PosSet = role match {
    case Pawn => pawn
    case Knight => knight
    case Bishop => bishop
    case Rook => rook
    case Queen => queen
    case King => king
  }

  def apply(piece: Piece): PosSet = apply(piece.color) & apply(piece.color)

  def has(pos: Pos): Boolean = occupied.has(pos)

  def place(piece: Piece, at: Pos): Option[PieceMap] =
    if (has(at)) None else Some(set(piece, at))

  def take(at: Pos): Option[PieceMap] =
    if (has(at)) Some(discard(at)) else None

  def set(piece: Piece, at: Pos): PieceMap = PieceMap(
    occupied = occupied + at,
    colors = colors.map(_ - at).update(piece.color, _ + at),
    pawn = if (piece.is(Pawn)) pawn + at else pawn - at,
    knight = if (piece.is(Knight)) knight + at else knight - at,
    bishop = if (piece.is(Bishop)) bishop + at else bishop - at,
    rook = if (piece.is(Rook)) rook + at else rook - at,
    queen = if (piece.is(Queen)) queen + at else queen - at,
    king = if (piece.is(King)) king + at else king - at
  )

  def discard(at: Pos): PieceMap = discardAll(PosSet(at))

  def discardAll(that: PosSet): PieceMap = PieceMap(
    occupied = occupied -- that,
    colors = colors.map(_ -- that),
    pawn = pawn -- that,
    knight = knight -- that,
    bishop = bishop -- that,
    rook = rook -- that,
    queen = queen -- that,
    king = king -- that
  )

  def kingOf(color: Color): Option[Pos] = apply(color.king).headOption

  def map[T](f: (Pos, Piece) => T): Map[Pos, T] = occupied.flatMap { pos =>
    apply(pos).map { piece =>
      pos -> f(pos, piece)
    }
  }.toMap

  def toMap = map((_, piece) => piece)

  def size = occupied.size
}

object PieceMap {
  val empty: PieceMap = PieceMap(
    PosSet.empty,
    Color.Map { _ => PosSet.empty },
    PosSet.empty,
    PosSet.empty,
    PosSet.empty,
    PosSet.empty,
    PosSet.empty,
    PosSet.empty
  )

  def from(iter: Iterable[(Pos, Piece)]): PieceMap = iter.foldLeft(empty) {
    case (acc, (pos, piece)) => acc.set(piece, pos)
  }
}
