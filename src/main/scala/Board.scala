package chess

import variant.{ Crazyhouse, Variant }

case class Board private (
    pieces: PieceMap,
    history: History,
    variant: Variant,
    crazyData: Option[Crazyhouse.Data] = None
) {

  def apply(at: Pos): Option[Piece] = pieces(at)
  def apply(file: File, rank: Rank): Option[Piece] = apply(Pos(file, rank))

  lazy val actors: Map[Pos, Actor] = pieces map {
    case (pos, piece) => Actor(piece, pos, this)
  }

  lazy val actorsOf: Color.Map[Seq[Actor]] = {
    val (w, b) = actors.values.toSeq.partition { _.color.white }
    Color.Map(w, b)
  }

  def actorAt(at: Pos): Option[Actor] = actors get at

  def rolesOf(c: Color): List[Role] = {
    val side = pieces(c)
    Role.all.filter(role => (side & pieces(role)).nonEmpty)
  }

  //def piecesOf(c: Color): Map[Pos, Piece] = pieces filter (_._2 is c)

  //lazy val kingPos: Map[Color, Pos] = pieces.collect {
  //  case (pos, Piece(color, King)) => color -> pos
  //}

  def kingPosOf(c: Color): Option[Pos] = pieces.kingOf(c)

  def check(c: Color): Boolean = c.fold(checkWhite, checkBlack)

  lazy val checkWhite = checkOf(White)
  lazy val checkBlack = checkOf(Black)

  private def checkOf(c: Color): Boolean =
    kingPosOf(c) exists { kingPos =>
      variant.kingThreatened(this, !c, kingPos)
    }

  def destsFrom(from: Pos): Option[List[Pos]] = actorAt(from) map (_.destinations)

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Option(this): Option[Board])(_ flatMap _)

  def place(piece: Piece, at: Pos): Option[Board] = pieces.place(piece, at).map(withPieces)

  def take(at: Pos): Option[Board] = pieces.take(at).map(withPieces)

  def move(orig: Pos, dest: Pos): Option[Board] =
    if (orig == dest) None
    else pieces(orig).flatMap { piece =>
      pieces.take(orig).flatMap(_.place(piece, dest))
    }.map(withPieces)

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] =
    for {
      piece <- pieces(orig)
      takenPos = taking getOrElse dest
      if pieces has takenPos
    } yield withPieces(pieces.discard(takenPos).discard(orig).set(piece, dest))

  def hasPiece(p: Piece) = pieces(p).nonEmpty

  def promote(pos: Pos): Option[Board] =
    for {
      pawn <- apply(pos)
      if pawn is Pawn
      b2 <- take(pos)
      b3 <- b2.place(pawn.color.queen, pos)
    } yield b3

  def castles: Castles = history.castles

  def withHistory(h: History): Board = copy(history = h)

  def withCastles(c: Castles) = withHistory(history withCastles c)

  def withPieces(newPieces: PieceMap) = copy(pieces = newPieces)

  def withVariant(v: Variant): Board = {
    if (v == Crazyhouse)
      copy(variant = v).ensureCrazyData
    else
      copy(variant = v)
  }

  def withCrazyData(data: Crazyhouse.Data)         = copy(crazyData = Option(data))
  def withCrazyData(data: Option[Crazyhouse.Data]) = copy(crazyData = data)
  def withCrazyData(f: Crazyhouse.Data => Crazyhouse.Data): Board =
    withCrazyData(f(crazyData | Crazyhouse.Data.init))

  def ensureCrazyData = withCrazyData(crazyData | Crazyhouse.Data.init)

  def unmovedRooks =
    UnmovedRooks {
      history.unmovedRooks.pos.filter(pos =>
        apply(pos).exists(piece => piece.is(Rook) && piece.color.backRank == pos.rank)
      )
    }

  def fixCastles: Board =
    withCastles {
      if (variant.allowsCastling) {
        val wkPos   = kingPosOf(White)
        val bkPos   = kingPosOf(Black)
        val wkReady = wkPos.fold(false)(_.rank == Rank.First)
        val bkReady = bkPos.fold(false)(_.rank == Rank.Eighth)
        def rookReady(color: Color, kPos: Option[Pos], left: Boolean) =
          kPos.fold(false) { kp =>
            actorsOf(color) exists { a =>
              a.piece.is(Rook) && a.pos ?- kp && (left ^ (a.pos ?> kp)) && history.unmovedRooks.pos(
                a.pos
              )
            }
          }
        Castles(
          whiteKingSide = castles.whiteKingSide && wkReady && rookReady(White, wkPos, left = false),
          whiteQueenSide = castles.whiteQueenSide && wkReady && rookReady(White, wkPos, left = true),
          blackKingSide = castles.blackKingSide && bkReady && rookReady(Black, bkPos, left = false),
          blackQueenSide = castles.blackQueenSide && bkReady && rookReady(Black, bkPos, left = true)
        )
      } else Castles.none
    }

  def updateHistory(f: History => History) = copy(history = f(history))

  def count(p: Piece): Int = pieces(p).size
  def count(c: Color): Int = pieces(c).size

  def autoDraw: Boolean =
    variant.fiftyMoves(history) || variant.isInsufficientMaterial(this) || history.fivefoldRepetition

  def situationOf(color: Color) = Situation(this, color)

  def visual = format.Visual >> this

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$variant Position after ${history.lastMove}\n$visual"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces, if (variant.allowsCastling) Castles.all else Castles.none, variant)

  def apply(pieces: Iterable[(Pos, Piece)], castles: Castles, variant: Variant): Board = {
    Board(PieceMap.from(pieces), History(castles = castles), variant, variantCrazyData(variant))
  }

  def init(variant: Variant): Board = Board(variant.pieces, variant.castles, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantCrazyData(variant: Variant) =
    (variant == Crazyhouse) option Crazyhouse.Data.init
}
