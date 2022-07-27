package test.nsinha.twobytwo

import nsinha.Utilities.{mapToAxes, printDerangedCubes, printDerangedPermCycles}
import nsinha.twobytwo.{DoB, DoBt, DoD, DoDt, DoF, DoFRFt, DoFRFtt, DoFt, DoL, DoLFLt, DoLt, DoR, DoRt, DoT, DoTt}
import nsinha.{Cube, Evaluation, MinusXOrientation, MinusZOrientation, MovedCube, Moves, RubiksCube, XAxis, XOrientation, YAxis, YOrientation, ZAxis, ZOrientation}
import org.scalatest.funspec.AnyFunSpec


class StrategyTopLayerSet extends AnyFunSpec {

  it("moves for perm of cycle 2 and diag elements switch") {
    val rubik = RubiksCube(2)
    val moves = Array(DoFRFt, DoD, DoFRFt, DoDt, DoR, DoFRFt, DoD, DoFRFt, DoDt, DoR, DoFRFt, DoD, DoFRFt, DoDt, DoR)

    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    rubik.findDisarrangedByPosCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //Set((1,0,1), (1,1,0))
    // ()            ()

  }






  it("two top ones on one side are rotated in place 132") {
    val rubik = RubiksCube(2)
    //L T L' T L T2 L'
    var moves = Array(DoF, DoR,DoF, DoR, DoF, DoR,DoF, DoR, DoF, DoR)

    moves.foreach(_.doMove(rubik))
    Array(DoT).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoTt).foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    // (010) (132) (011)(132)

  }

  it("two top ones on one diag are rotated in place and 123,132") {
    val rubik = RubiksCube(2)
    var moves = Array(DoF, DoR,DoF, DoR, DoF, DoR,DoF, DoR, DoF, DoR)

    moves.foreach(_.doMove(rubik))
    Array(DoTt).foreach(_.doMove(rubik))
    Array(DoTt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoT).foreach(_.doMove(rubik))
    Array(DoT).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    // (010) (123) (111)(132)

  }

  it("two on main diagonal are rotated in place 132-2") {
    val rubik = RubiksCube(2)
    //L T L' T L T2 L'
    var moves = Array(DoF, DoR,DoF, DoR, DoF, DoR,DoF, DoR, DoF, DoR)

    Array(DoL).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoTt).foreach(_.doMove(rubik))
    Array(DoTt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoT).foreach(_.doMove(rubik))
    Array(DoT).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoLt).foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    // (000) (132) (111)(132)

  }

  it("three top ones  are rotated in place- a variation of two top ones on one side are rotated in place") {
    val rubik = RubiksCube(2)
    //L T L' T L T2 L'
    var moves = Array(DoF, DoR,DoF, DoR, DoF, DoR,DoF, DoR, DoF, DoR)
    var movesBringLeftOver = Array(DoT)
    var movesBringLeftOverOpp = Array(DoTt)

    moves.foreach(_.doMove(rubik))
    Array(DoT).foreach(_.doMove(rubik))
    Array(DoT).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoTt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoTt).foreach(_.doMove(rubik))


    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //(010) (132) (011)(123)  (111)(132)

  }

  it("three top ones  are rotated in place- a variation of two top ones on one side are rotated in place-1") {
    val rubik = RubiksCube(2)
    //L T L' T L T2 L'
    var moves = Array(DoFt, DoRt,DoFt, DoRt, DoFt, DoRt,DoFt, DoRt, DoFt, DoRt)
    var movesBringLeftOver = Array(DoT)
    var movesBringLeftOverOpp = Array(DoTt)

    moves.foreach(_.doMove(rubik))
    movesBringLeftOver.foreach(_.doMove(rubik))
    movesBringLeftOver.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    movesBringLeftOverOpp.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    movesBringLeftOverOpp.foreach(_.doMove(rubik))


    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //(010) (123) (011)(132)  (111)(123)

  }

  it("3 r cubes moved in sequence with (123)(23)(12)") {
    val rubik = RubiksCube(2)
    //L T L' T L T2 L'
    var moves = Array(DoF, DoR,DoFt)

    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))


    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //(1,0,1), (1,1,1), (1,1,0))
    //(123)    (23)     (12)
  }


  it("3 r cubes moved in place  with last R rot ") {
    val rubik = RubiksCube(2)
    //L T L' T L T2 L'
    var moves = Array(DoF, DoR,DoFt)

    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))

    Array(DoR).foreach(_.doMove(rubik))


    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //(1,0,1), (1,0,0)
    // (13)      (23)
    // (1,1,0)
    //(132)


  }


  it("3 r cubes moved in place with a anomalos twist  with last R rot ^ 2") {
    val rubik = RubiksCube(2)
    //L T L' T L T2 L'
    var moves = Array(DoF, DoR,DoFt)

    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoR).foreach(_.doMove(rubik))

    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoR).foreach(_.doMove(rubik))

    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoR).foreach(_.doMove(rubik))

    moves.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoDt).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    Array(DoR).foreach(_.doMove(rubik))


    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //(1,0,1), (1,0,0)
    // (13)      (23)
    // (1,1,0)
    //(132)


  }




  it("hgfhfbgghc") {
    val rubik = RubiksCube(2)
    //L T L' T L T2 L'
    var moves = Array(DoF, DoR,DoFt, DoRt)
    var movesSet = Array(DoD, DoD)
    var movesOpp = Array(DoR, DoF,DoRt, DoFt)

    moves.foreach(_.doMove(rubik))
    movesSet.foreach(_.doMove(rubik))
    movesOpp.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    movesSet.foreach(_.doMove(rubik))
    movesOpp.foreach(_.doMove(rubik))
    Array(DoD).foreach(_.doMove(rubik))


    rubik.findDisarrangedCubes().map(c => println(c))
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //, (1,0,0), (1,0,1)  (0,0,1))
    // (132)      (123)       (132)

  }





}
