package test.nsinha

import nsinha.{Derangement, Moves, RubiksCube, XAxis, YAxis, ZAxis}
import nsinha.Utilities.{frequencyChartMoves, getRandomCubePos, getRandomMove, printDerangedCubes}
import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable
import Console.{GREEN, RED, RESET, UNDERLINED, YELLOW_B}
import scala.collection.mutable.ListBuffer
import scala.util.Random

class TestRubiksCubeMovesComplex extends AnyFunSpec {
  val rand = new Random(System.nanoTime())


  it ("check moves in random seq and reverse makes the rubiks ok 1") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(XAxis,YAxis, (0,0,0))
    rubik.makeMove(XAxis,ZAxis, (1,1,1))
    rubik.makeMove(ZAxis,YAxis, (2,2,2))
    rubik.makeMove(XAxis,YAxis, (1,0,1))
    //reverse
    rubik.makeMove(YAxis,XAxis, (1,0,1))
    rubik.makeMove(YAxis,ZAxis, (2,2,2))
    rubik.makeMove(ZAxis,XAxis, (1,1,1))
    rubik.makeMove(YAxis,XAxis, (0,0,0))
    assert (rubik.findDisarrangedCubes().size == 0)
  }


  it ("check moves in random seq and reverse makes the rubiks ok 2") {
    for (i <- Range(0, 10)) {
      val rubik = new RubiksCube(3)
      val moves = ListBuffer[(Moves, (Int, Int, Int))]()
      for (j <- Range(0, 5)) {
        val move: Moves = getRandomMove()
        val pos = getRandomCubePos(3)
        moves += ((move, pos))
      }
      val movesRev = moves.reverse
      for (m <- moves) {
        if (m._1.a != m._1.b) {
          rubik.makeMove(m._1.a, m._1.b, m._2)
        }
      }
      for (m <- movesRev) {
        if (m._1.a != m._1.b) {
          rubik.makeMove(m._1.b, m._1.a, m._2)
        }
      }
      assert(rubik.findDisarrangedCubes().size == 0)
    }
  }

  it("findFixForCube  for three turns on X->Y, X->Z") {
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
