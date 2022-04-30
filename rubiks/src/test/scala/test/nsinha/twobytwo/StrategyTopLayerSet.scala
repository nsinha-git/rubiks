package test.nsinha.twobytwo

import nsinha.Utilities.{mapToAxes, printDerangedCubes, printDerangedPermCycles}
import nsinha.twobytwo.NamedMove.opposite
import nsinha.twobytwo.{DoB, DoBt, DoD, DoDt, DoF, DoFRFt, DoFRFtt, DoFt, DoL, DoLFLt, DoLt, DoR, DoRt, DoT, DoTt}
import nsinha.{Cube, Evaluation, MinusXOrientation, MinusZOrientation, MovedCube, Moves, RubiksCube, XAxis, XOrientation, YAxis, YOrientation, ZAxis, ZOrientation}
import org.scalatest.funspec.AnyFunSpec

import scala.Console.CYAN
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class StrategyTopLayerSet extends AnyFunSpec {

  it("moves for perm of cycle 2") {
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

  }


  it("moves for perm of cycle 2-2") {
    val rubik = RubiksCube(2)
    val moves = Array(DoFRFt, DoL, DoFRFt, DoFRFt, DoL, DoFRFt, DoFRFt, DoL, DoFRFt, DoFRFt, DoL, DoFRFt, DoFRFt, DoL, DoFRFt)
    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    rubik.findDisarrangedByPosCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    //Set((0,1,1), (1,0,0))

  }

  it("moves for perm of perms -2 x 5  ") {
    val rubik = RubiksCube(2)
    val moves = Array(DoFRFt, DoL, DoFRFt)
    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    rubik.findDisarrangedByPosCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //HashSet((1,1,0), (0,0,0), (0,0,1), (0,1,0), (1,1,1))
    //Set((0,1,1), (1,0,0))

  }

  it("moves for perm of perms -1 x 4 1 ") {
    val rubik = RubiksCube(2)
    val moves = Array(DoFRFt, DoR, DoFRFt)
    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    rubik.findDisarrangedByPosCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //Set((1,0,0))
    //Set((0,1,1), (1,0,1), (1,1,1), (1,1,0))
  }

  it("moves for perm of perms -1 x 4 1  4 times") {
    val rubik = RubiksCube(2)
    val moves = Array(DoFRFt, DoR, DoFRFt)
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    rubik.findDisarrangedByPosCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //Set((1,0,0))
    //Set((0,1,1), (1,0,1), (1,1,1), (1,1,0))
  }

  it("moves for perm of perms 4") {
    val rubik = RubiksCube(2)
    val moves = Array(DoR, DoFRFt, DoRt)
    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    rubik.findDisarrangedByPosCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //Set((0,1,1), (1,0,1), (1,1,1), (1,1,0))
  }

  it("moves for perm of perms 6 ") {
    val rubik = RubiksCube(2)
    val moves = Array(DoFRFt, DoR, DoFRFtt)
    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    rubik.findDisarrangedByPosCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //HashSet((1,1,0), (1,0,1), (0,1,1), (1,1,1), (1,0,0), (0,0,1))
  }


  it("moves for perm of perms ... ") {
    val rubik = RubiksCube(2)
    var moves = Array(DoFRFt, DoR, DoFRFtt)
    moves.foreach(_.doMove(rubik))
    moves = Array(DoR, DoFRFt, DoRt)
    moves.foreach(_.doMove(rubik))


    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    rubik.findDisarrangedByPosCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //HashSet((1,1,0), (1,0,1), (0,1,1), (1,1,1), (1,0,0), (0,0,1))
  }


  it("moves for perm of perms 1 ") {
    val rubik = RubiksCube(2)
    //L T L' T L T2 L'
    var moves = Array(DoL, DoT, DoLt, DoT, DoL, DoT, DoT, DoLt)
    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //HashSet((1,1,0), (1,0,1), (0,1,1), (1,1,1), (1,0,0), (0,0,1))
  }











}
