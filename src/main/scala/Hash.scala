package chess

import java.security.MessageDigest

object Hash {

  private[chess] val size = 3

  private def apply(str: String): PositionHash =
    MessageDigest getInstance "MD5" digest (str getBytes "UTF-8") take size

  def apply(board: Board, color: Color): PositionHash = apply {
    board.actors.values.map { a =>
      s"${a.piece.forsyth}${a.pos.key}"
    }.mkString + color.letter
  }

  def debug(hashes: PositionHash) = hashes.map(_.toInt).sum.toString
}
