package nsinha

case class Moves(a: Axis, b: Axis) {
  override def toString: String = {
    a.toString + "," + b.toString
  }
}
object MoveXY extends Moves(XAxis, YAxis)
object MoveYX extends Moves(YAxis, XAxis)
object MoveXZ extends Moves(XAxis, ZAxis)
object MoveZX extends Moves(ZAxis, XAxis)
object MoveYZ extends Moves(YAxis, ZAxis)
object MoveZY extends Moves(ZAxis, YAxis)

object Moves {
  def allMoves(): List[Moves] = {
    List(MoveXY, MoveXZ, MoveYX, MoveYZ, MoveZX, MoveZY)
  }

  def oppositeMoves(a: Option[Moves], b: Moves): Boolean = {
    if (a.isEmpty) return false
    (a.get, b) match {
      case (MoveXY, MoveYX) => true
      case (MoveYX, MoveXY) => true
      case (MoveXZ, MoveZX) => true
      case (MoveZX, MoveXZ) => true
      case (MoveYZ, MoveZY) => true
      case (MoveZY, MoveYZ) => true
      case  _ => false
    }

  }

}
