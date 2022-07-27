package test.nsinha.twobytwo
import nsinha.Utilities.{mapToAxes, printDerangedCubes, printDerangedPermCycles}
import nsinha.twobytwo.{DoB, DoBt, DoD, DoDt, DoF, DoFRFt, DoFRFtt, DoFt, DoL, DoLFLt, DoLt, DoR, DoRt, DoT, DoTt}
import nsinha.{Cube, Evaluation, MinusXOrientation, MinusZOrientation, MovedCube, Moves, RubiksCube, XAxis, XOrientation, YAxis, YOrientation, ZAxis, ZOrientation}
import org.scalatest.funspec.AnyFunSpec

class ImportantStrategy extends AnyFunSpec {

  it("FR moves 5 times two top ones on one side are rotated in place 123 -t1") {
    val rubik = RubiksCube(2)
    var moves = Array(DoF, DoR,DoF, DoR, DoF, DoR,DoF, DoR, DoF, DoR)

    moves.foreach(_.doMove(rubik))


    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //List(((1,1,0),List(1, 3, 2)))
    //List(((0,1,1),List(1, 3, 2)))
    //List(((1,1,1),List(1, 2, 3)))
    //List(((1,0,1),List(1, 3, 2)))
    //List(((0,0,1),List(1, 2, 3)))
    //List(((1,0,0),List(1, 2, 3)))

  }

  it("FR moves 5 times two top ones on one side are rotated in place 123") {
    val rubik = RubiksCube(2)
    var moves = Array(DoF, DoR,DoF, DoR, DoF, DoR,DoF, DoR, DoF, DoR)

    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))




    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //PERM CYCLES <
    //List(((0,0,0),List(1, 3, 2)))
    //List(((1,0,0),List(1, 3, 2)))
    //PERM CYCLES/>

  }


  it("FR moves 5 times diag cubes are rotated in place") {
    val rubik = RubiksCube(2)
    var moves = Array(DoF, DoR,DoF, DoR, DoF, DoR,DoF, DoR, DoF, DoR)

    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))

    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //PERM CYCLES <
    //List(((0,0,0),List(1, 3, 2)))
    //List(((1,0,1),List(1, 2, 3)))
    //PERM CYCLES/>

  }

  it("FR moves 5 times 3 same face cube rot move at same pos") {
    val rubik = RubiksCube(2)
    var moves = Array(DoF, DoR,DoF, DoR, DoF, DoR,DoF, DoR, DoF, DoR)

    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))

    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //((0,0,0),List(1, 2, 3)))
    //List(((0,0,1),List(1, 3, 2)))
    //List(((1,0,0),List(1, 3, 2)))

  }




  it("FRFt moves two cubes") {
    val rubik = RubiksCube(2)
    //var moves = Array(DoF, DoR, DoFt) //List(((0,1,1),List(1, 3)), ((1,1,1),List(1, 2, 3)), ((1,0,0),List(2, 3)), ((1,1,0),List(1, 3, 2)))
    var moves = Array(DoF,DoR, DoFt, DoRt,DoD, DoD,DoF,DoR, DoFt, DoRt,DoD, DoD)
    //List(((0,1,1),List(1, 3)), ((1,1,1),List(1, 2)))
    //List(((1,0,1),List(1, 3)), ((1,0,0),List(2, 3)))
    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))


    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //(1,0,1),List(2, 3)), ((0,0,0),List(2, 3)))
  }
}
