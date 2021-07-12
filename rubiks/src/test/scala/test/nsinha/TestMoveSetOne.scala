package test.nsinha


import test.nsinha.Utilities.{frequencyChartMoves, printDerangedCubes}
import org.scalatest.funspec.AnyFunSpec

import scala.Console.{BLUE, CYAN, MAGENTA, RESET, YELLOW};

class TestMoveSetOne extends AnyFunSpec {
  it("findFixForCube  for three turns on Y->X, X->Z") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(YAxis, XAxis, (0, 0, 0))
    printDerangedCubes(rubik)
    rubik.findDisarrangedCubes().foreach({ x =>
      val derangement = Derangement.getDerangementOfCube(x, 3)
      val moves = Derangement.getMovesForOrientation(derangement)
      println(x)
      moves.foreach(println(_))
      println()
    })
    rubik.makeMove(XAxis, ZAxis, (0, 1, 0))
    printDerangedCubes(rubik)
    val moves = rubik.findDisarrangedCubes().map({ x =>
      val derangement = Derangement.getDerangementOfCube(x, 3)
      val moves = Derangement.getMovesForOrientation(derangement)
      println(x)
      moves.foreach(println(_))
      println()
      moves
    })

    val freqMapMoves = frequencyChartMoves(moves)
    println(freqMapMoves)

    val rubik1 = rubik.copy()
    val rubik2 = rubik.copy()
    println(s"${RESET}${MAGENTA}")
    printDerangedCubes(rubik)
    println(s"${RESET}${BLUE}")
    printDerangedCubes(rubik1)
    println(s"${RESET}${CYAN}")
    printDerangedCubes(rubik2)


  }








}
