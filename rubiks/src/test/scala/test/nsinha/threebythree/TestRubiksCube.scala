package test.nsinha.threebythree

import nsinha.Utilities.{fixOrientation, getTupleValAt, move, orient}
import nsinha._
import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable

class TestRubiksCube extends AnyFunSpec {

  it("create Rubiks cube and check no one is deranaged") {
    val rubik = new RubiksCube(3)
    assert(rubik.findDisarrangedCubes().isEmpty)
  }

  it("check get Tuple value ") {
    assert (getTupleValAt((5,6,7), 1) == 5)
    assert (getTupleValAt((5,6,7), 2) == 6)
    assert (getTupleValAt((5,6,7), 3) == 7)
  }

  it("test move a single pos 0,0,0") {
    assert (move((0,0,0), 1, 1, 2, 3) == 2)
    assert (move((0,0,0), 2, 1, 2, 3) == 0)
    assert (move((0,0,0), 3, 1, 2, 3) == 0)

  }

  it("test move a single pos 1,0,0") {
    assert (move((1,0,0), 1, 1, 2, 3) == 2)
    assert (move((1,0,0), 2, 1, 2, 3) == 1)
    assert (move((1,0,0), 3, 1, 2, 3) == 0)

  }

  it("test move a single pos 2,0,0") {
    assert (move((2,0,0), 1, 1, 2, 3) == 2)
    assert (move((2,0,0), 2, 1, 2, 3) == 2)
    assert (move((2,0,0), 3, 1, 2, 3) == 0)
  }

  it("cube orientation") {
    val cube = new Cube(0,0,0,0,0,0)
    fixOrientation(cube, 1, 2)
    assert (cube.orientX == YOrientation)
    assert (cube.orientY == MinusXOrientation)
    assert (cube.orientZ == ZOrientation)

  }


  it("makeMove") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(ZAxis,YAxis, (2,0,0))
    assert (rubik.findDisarrangedCubes().size == 9)
  }

  it("test orient") {
    assert (orient(XOrientation, 2,3) == XOrientation)
    assert (orient(MinusZOrientation, 1,2) == MinusZOrientation)
    assert (orient(XOrientation, 1,3) == ZOrientation)
    assert (orient(MinusXOrientation, 1,3) == MinusZOrientation)

  }

  it("findFixForCube  for single turn on Z->Y") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(ZAxis,YAxis, (2,0,0))
    val derangedCubes = rubik.findDisarrangedCubes()
    val cube1 = derangedCubes(0)
    val cube2 = derangedCubes(5)
    val listOfPaths_1 = rubik.findFixForCube(cube1, 6).toList.map(x => x.toList)
    val listOfPaths_2 = rubik.findFixForCube(cube2, 6).toList.map(x => x.toList)

    val nextMove = rubik.findMostCommonFix(Map(cube1 -> listOfPaths_1, cube2 -> listOfPaths_2))
    assert(nextMove.nonEmpty)
    assert(nextMove.head._1.a == YAxis && nextMove.head._1.b == ZAxis)

  }

  it("findFixForCube  for single turn on X->Y") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(XAxis,YAxis, (1,0,0))
    val derangedCubes = rubik.findDisarrangedCubes()

    val moves = for (cube <- derangedCubes) yield {
      val listPaths = rubik.findFixForCube(cube, 6).toList.map(_.toList)
      cube -> listPaths
    }
    val nextMove = rubik.findMostCommonFix(moves.toMap)
    assert(nextMove.nonEmpty)
    assert(nextMove.head._1.a == YAxis && nextMove.head._1.b == XAxis)
  }

  it("findFixForCube  for two turns on X->Y, X-Y") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(XAxis,YAxis, (1,0,0))
    rubik.makeMove(XAxis,YAxis, (1,0,0))
    val derangedCubes = rubik.findDisarrangedCubes()
    assert (derangedCubes.size == 9)

    val moves = for (cube <- derangedCubes) yield {
      val listPaths = rubik.findFixForCube(cube, 6).toList.map(_.toList)
      cube -> listPaths
    }
    val nextMove = rubik.findMostCommonFix(moves.toMap)
    assert(nextMove.nonEmpty)
    assert((nextMove.head._1.a == YAxis && nextMove.head._1.b == XAxis) ||  (nextMove.head._1.a == XAxis && nextMove.head._1.b == YAxis) )
  }


  it("findFixForCube  for two turns on X->Y, Z->X") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(XAxis,YAxis, (1,1,1))
    rubik.makeMove(XAxis,YAxis, (0,0,0))
    var derangedCubes = rubik.findDisarrangedCubes()
    assert (derangedCubes.size == 18)

    var moves = derangedCubes.foldLeft(new mutable.HashMap[Cube, List[List[Moves]]]) { (Z, cube) =>
      val listPaths = rubik.findFixForCube(cube, 6).toList.map(_.toList)
      Z(cube) = listPaths
      Z
    }.toMap


    var nextMove = rubik.findMostCommonFix(moves)
    assert(nextMove.nonEmpty)
    var (concreteMove, concreteCube) = nextMove.get
    rubik.makeMove(concreteMove.a, concreteMove.b, (concreteCube.currX, concreteCube.currY, concreteCube.currZ))
    derangedCubes = rubik.findDisarrangedCubes()
    assert (derangedCubes.size == 9)

    moves = derangedCubes.foldLeft(new mutable.HashMap[Cube, List[List[Moves]]]) { (Z, cube) =>
      val listPaths = rubik.findFixForCube(cube, 6).toList.map(_.toList)
      Z(cube) = listPaths
      Z
    }.toMap

    nextMove = rubik.findMostCommonFix(moves)
    assert(nextMove.nonEmpty)
    var (concreteMove1, concreteCube1) = nextMove.get
    rubik.makeMove(concreteMove1.a, concreteMove1.b, (concreteCube1.currX, concreteCube1.currY, concreteCube1.currZ))
    derangedCubes = rubik.findDisarrangedCubes()
    assert (derangedCubes.size == 0)
  }



}
