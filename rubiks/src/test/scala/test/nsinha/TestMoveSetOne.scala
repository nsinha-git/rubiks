package test.nsinha


import nsinha.{Derangement, Evaluation, RubiksCube, XAxis, YAxis, ZAxis}
import nsinha.Utilities.{frequencyChartMoves, getTopFrequenciesMove, printDerangedCubes}
import org.scalatest.funspec.AnyFunSpec

import scala.Console.{BLUE, CYAN, MAGENTA, RESET, YELLOW};

class TestMoveSetOne extends AnyFunSpec {
  it("findFixForCube  for three turns on Y->X, X->Z") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(YAxis, XAxis, (0, 0, 0)) //minusZ
    rubik.makeMove(XAxis, ZAxis, (0, 1, 0)) //minusY
    rubik.makeMove(YAxis, ZAxis, (0, 1, 1)) //X

    val ev = Evaluation.evaluate(rubik, 6, List.empty)
    assert(ev._1)
    println(s"${CYAN}")
    println(ev._3)
  }


}
