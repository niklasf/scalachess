package chess

/**
  * Utility methods for helping to determine whether a situation is a draw or a draw
  * on a player flagging.
  *
  * See http://www.e4ec.org/immr.html
  */
object InsufficientMatingMaterial {

  def bishopsOnOppositeColors(board: Board) =
    (board.pieces.bishop & PosSet.lightSquares).nonEmpty &&
    (board.pieces.bishop & PosSet.darkSquares).nonEmpty

  /*
   * Returns true if a pawn cannot progress forward because it is blocked by a pawn
   */
  def pawnBlockedByPawn(pawn: Actor, board: Board) =
    pawn.moves.isEmpty && {
      val blockingPosition = Actor.posAheadOfPawn(pawn.pos, pawn.piece.color)
      blockingPosition.flatMap(board.apply).exists(_.is(Pawn))
    }

  /*
   * Determines whether a board position is an automatic draw due to neither player
   * being able to mate the other as informed by the traditional chess rules.
   */
  def apply(board: Board) = {
    val kingsAndBishopsOnly = board.pieces.occupied == (board.pieces.king | board.pieces.bishop)
    val kingsAndMinorsOnly = board.pieces.occupied == (board.pieces.king | board.pieces.bishop | board.pieces.knight)
    kingsAndMinorsOnly && (board.pieces.occupied.size <= 3 || (kingsAndBishopsOnly && !bishopsOnOppositeColors(board)))
  }

  /*
   * Determines whether a color does not have mating material. In general:
   * King by itself is not mating material
   * King + knight mates against king + any(rook, bishop, knight, pawn)
   * King + bishop mates against king + any(bishop, knight, pawn)
   * King + bishop(s) versus king + bishop(s) depends upon bishop square colors
   */
  def apply(board: Board, color: Color) = {
    val kingsAndMinorsOnlyOfColor = board.pieces(color) == (board.pieces.king | board.pieces.bishop | board.pieces.knight)
    lazy val nonKingRolesOfColor  = board rolesOf color filter (King !=)
    lazy val rolesOfOpponentColor = board rolesOf !color

    kingsAndMinorsOnlyOfColor && (nonKingRolesOfColor.distinct match {
      case Nil => true
      case List(Knight) =>
        nonKingRolesOfColor.lengthCompare(
          1
        ) == 0 && !(rolesOfOpponentColor filter (King !=) exists (Queen !=))
      case List(Bishop) =>
        !(rolesOfOpponentColor.exists(r => r == Knight || r == Pawn) || bishopsOnOppositeColors(board))
      case _ => false
    })
  }
}
