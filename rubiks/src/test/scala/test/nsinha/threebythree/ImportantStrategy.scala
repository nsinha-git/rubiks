package test.nsinha.threebythree

import nsinha.RubiksCube
import nsinha.Utilities.{printDerangedCubes, printDerangedPermCycles}
import nsinha.twobytwo.{DoD, DoDt, DoF, DoFt, DoMF, DoML, DoMR, DoMRt, DoR, DoRt, DoT, DoTt}
import org.scalatest.funspec.AnyFunSpec

class SamePlaceStrategy extends AnyFunSpec {
  it("FR moves 5 times two top ones on one side are rotated in place 123") {
    val rubik = RubiksCube(3)
    val moves = Array(DoF, DoR, DoF, DoR, DoF, DoR, DoF, DoR, DoF,DoR)
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))

    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    /**
     * List(((2,2,0),List(1, 3, 2)))
List(((2,0,2),List(1, 3, 2)))
List(((0,0,2),List(1, 2, 3)))
List(((2,1,1),List(2, 3)))
List(((2,0,0),List(1, 2, 3)))
List(((0,2,2),List(1, 3, 2)))
List(((2,2,2),List(1, 2, 3)))
List(((1,1,2),List(1, 2)))
     */

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
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))



    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //PERM CYCLES <
    //List(((1,1,2),List(1, 2)))
    //List(((2,1,1),List(2, 3)))
    //PERM CYCLES/>
  }
}

class ImportantStrategy extends AnyFunSpec {
  it("FRFtRt moves two pairs ^2 move") {
    val rubik = RubiksCube(3)
    val moves = Array(DoF, DoR, DoFt, DoRt)
    moves.foreach(_.doMove(rubik))

    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

    /**
     * PERM CYCLES <
List(((2,0,0),List(2, 3)), ((2,0,2),List(1, 3)))
List(((2,1,2),List(1, 2)), ((1,2,2),List(1, 2, 3)), ((2,0,1),List(2, 3)))
List(((0,2,2),List(1, 3)), ((2,2,2),List(1, 2)))
PERM CYCLES/>
     */

    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))

    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //PERM CYCLES <
    //List(((0,2,2),List(2, 3)), ((2,2,2),List(2, 3)))
    //List(((2,0,0),List(1, 2)), ((2,0,2),List(1, 2)))
    //PERM CYCLES/>

  }


  it("FRFtRt moves two pairs ^2 move - DoTsq Var") {
    val rubik = RubiksCube(3)
    val moves = Array(DoF, DoR, DoFt, DoRt)
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))

    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //PERM CYCLES <
    //List(((0,2,2),List(2, 3)), ((2,2,2),List(2, 3)))
    //List(((2,0,0),List(1, 2)), ((2,0,2),List(1, 2)))
    //PERM CYCLES/>

  }

  it("FRFtRt moves two pairs ^2 move - DoTsq Var") {
    val rubik = RubiksCube(3)
    val moves = Array(DoF, DoR, DoFt, DoRt)
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))
    moves.foreach(_.doMove(rubik))

    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()
    //PERM CYCLES <
    //List(((0,2,2),List(2, 3)), ((2,2,2),List(2, 3)))
    //List(((2,0,0),List(1, 2)), ((2,0,2),List(1, 2)))
    //PERM CYCLES/>

  }



}
