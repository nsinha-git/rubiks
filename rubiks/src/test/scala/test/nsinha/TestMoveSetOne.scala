package test.nsinha


import test.nsinha.Utilities.{frequencyChartMoves, printDerangedCubes}
import org.scalatest.funspec.AnyFunSpec;

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

    rubik.makeMove(XAxis, ZAxis, (0, 1, 0))

  }




}
