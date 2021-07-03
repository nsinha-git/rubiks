package nsinha

case class MoveUnion(moves: List[Moves]) {
}


object MoveUnion {
  def intersect(a: MoveUnion, b: MoveUnion) : MoveUnion = {
    if (a.moves != null) {
      MoveUnion(a.moves.toSet.intersect(b.moves.toSet).toList)
    } else {
      b
    }
  }

}
