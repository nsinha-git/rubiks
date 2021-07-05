package nsinha

import nsinha.Utilities.printDerangedCubes
import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable
import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED}

class TestRubiksCubeMovesComplex extends AnyFunSpec {

  it("findFixForCube  for three turns on X->Y, X->Y, X->Y") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(XAxis,YAxis, (0,0,0))
    rubik.makeMove(YAxis,ZAxis, (0,0,0))
    rubik.makeMove(XAxis,YAxis, (0,0,0))

    var cycle = 1
    var cond = true
    while (rubik.findDisarrangedCubes().size != 0 && cond == true) {
      printDerangedCubes(rubik)
      println("Moves are generated for cycle = " + cycle)
      cycle += 1
      println("rubiks has deranged size=" + rubik.findDisarrangedCubes().size)

      val moves = rubik.findDisarrangedCubes().foldLeft(new mutable.HashMap[Cube, List[List[Moves]]]) { (Z, cube) =>
        val listPaths = rubik.findFixForCube(cube).toList.map(_.toList)
        Z(cube) = listPaths
        Z
      }.toMap

      val nextMove = rubik.findMostCommonFix(moves)

      if (nextMove.isEmpty) {
        cond = false
      } else {
        assert(nextMove.nonEmpty)
        val (concreteMove, concreteCube) = nextMove.get
        println(s"${RESET}${RED}Move is calculated. Making a move ${concreteMove} at ${concreteCube.currX}, ${concreteCube.currY}, ${concreteCube.currZ}${RESET}")
        rubik.makeMove(concreteMove.a, concreteMove.b, (concreteCube.currX, concreteCube.currY, concreteCube.currZ))
      }
    }
  }




}
