package nsinha

import nsinha.Utilities.{fixOrientation, getTupleValAt, move, orient}
import org.scalatest.funspec.AnyFunSpec

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

  it("getSlice") {
    val rubik = new RubiksCube(3)
    val slice = rubik.getSlice(1, 0)
    assert (slice.size == 9)

  }

  it("makeMove") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(ZAxis,YAxis, (2,0,0))
    assert (rubik.findDisarrangedCubes().size == 9)
  }

  it("test orient") {
    assert (orient(XOrientation, YOrientation) == MinusZOrientation)
    assert (orient(YOrientation, MinusXOrientation) == MinusZOrientation)
    assert (orient(ZOrientation, MinusXOrientation) == MinusZOrientation)

  }

  it("findFixForCube ") {
    val rubik = new RubiksCube(3)
    rubik.makeMove(ZAxis,YAxis, (2,0,0))
    val derangedCubes = rubik.findDisarrangedCubes()
    val cube1 = derangedCubes(0)
    val cube2 = derangedCubes(5)
    val listOfPaths_1 = rubik.findFixForCube(cube1).toList.map(x => x.toList)
    val listOfPaths_2 = rubik.findFixForCube(cube2).toList.map(x => x.toList)

    val nextMove = rubik.findMostCommonFix(Map(cube1 -> listOfPaths_1, cube2 -> listOfPaths_2))
    println(nextMove)

  }


}
