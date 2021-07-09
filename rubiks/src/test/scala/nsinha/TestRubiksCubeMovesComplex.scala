package nsinha

import nsinha.Utilities.printDerangedCubes
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

  def getRandomMove(): Moves = {
   Moves(intToOrientation(rand.nextInt(3)), intToOrientation(rand.nextInt(3)))
  }

  def getRandomCubePos(n: Int): (Int, Int, Int) = {
    (rand.nextInt(3), rand.nextInt(3), rand.nextInt(3))
  }

  def intToOrientation(i: Int): Axis = {
    i match {
      case 0 => XAxis
      case 1 => YAxis
      case 2 => ZAxis
    }
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
      //rubik.makeMove(XAxis, YAxis, (0,0,0))
      assert(rubik.findDisarrangedCubes().size == 0)
    }
  }

  it("findFixForCube  for three turns on X->Y, X->Z") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(XAxis,YAxis, (0,0,0))

    var cycle = 1
    var cond = true
    while (rubik.findDisarrangedCubes().size != 0 && cond == true) {
      printDerangedCubes(rubik)
      rubik.findDisarrangedCubes().foreach(x => println(Derangement.getDerangementOfCube(x, 3)))
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
