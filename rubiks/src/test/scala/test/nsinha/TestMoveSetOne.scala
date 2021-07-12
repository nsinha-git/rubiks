package test.nsinha


import nsinha.{Derangement, RubiksCube, XAxis, YAxis, ZAxis}
import nsinha.Utilities.{frequencyChartMoves, printDerangedCubes}
import org.scalatest.funspec.AnyFunSpec

import scala.Console.{BLUE, CYAN, MAGENTA, RESET, YELLOW};

class TestMoveSetOne extends AnyFunSpec {
  it("findFixForCube  for three turns on Y->X, X->Z") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(YAxis, XAxis, (0, 0, 0)) //minusZ
    printDerangedCubes(rubik)
    rubik.findDisarrangedCubes().foreach({ x =>
      val derangement = Derangement.getDerangementOfCube(x, 3)
      val moves = Derangement.getMovesForOrientation(derangement)
      println(x)
      moves.foreach(println(_))
      println()
    })
    rubik.makeMove(XAxis, ZAxis, (0, 1, 0))//minusY
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
    rubik.makeMove(ZAxis, YAxis, (2,1,0)) //MinusXrot
    println(s"${CYAN}")

    printDerangedCubes(rubik)
    val moves1 = rubik.findDisarrangedCubes().map({ x =>
      val derangement = Derangement.getDerangementOfCube(x, 3)
      val moves = Derangement.getMovesForOrientation(derangement)
      println(x)
      moves.foreach(println(_))
      println()
      moves
    })

    val freqMapMoves1 = frequencyChartMoves(moves1)
    println(freqMapMoves1)



  }








}
