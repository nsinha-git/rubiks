package test.nsinha.threebythree

import nsinha.Utilities.{getFreqDistMoves, printDerangedCubes}
import nsinha._
import org.scalatest.funspec.AnyFunSpec

import scala.Console.CYAN;

class TestMoveSetOne extends AnyFunSpec {
  it("findFixForCube  for three turns on Y->X, X->Z") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(YAxis, XAxis, (0, 0, 0)) //minusZ
    printDerangedCubes(rubik)
    println(getFreqDistMoves(rubik)._2)
    rubik.makeMove(XAxis, ZAxis, (0, 1, 0)) //minusY
    printDerangedCubes(rubik)
    println(getFreqDistMoves(rubik)._2)
    rubik.makeMove(YAxis, ZAxis, (2, 2, 1)) //X
    printDerangedCubes(rubik)
    println(getFreqDistMoves(rubik)._2)
    rubik.makeMove(XAxis, YAxis, (0, 0, 2)) //Z
    printDerangedCubes(rubik)
    println(getFreqDistMoves(rubik)._2)
    rubik.makeMove(ZAxis, XAxis, (0, 0, 1)) //Y
    printDerangedCubes(rubik)
    println(getFreqDistMoves(rubik)._2)

    val ev = Evaluation.evaluate(rubik, 6, List.empty)
    assert(ev._1)
    println(s"${CYAN}")
    println(ev._3)
  }


}
