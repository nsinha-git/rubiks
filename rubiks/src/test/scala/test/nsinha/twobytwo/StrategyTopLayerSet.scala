package test.nsinha.twobytwo

import nsinha.Utilities.{mapToAxes, printDerangedCubes, printDerangedPermCycles}
import nsinha.twobytwo.NamedMove.opposite
import nsinha.twobytwo.{DoD, DoDt, DoFRFt, DoL, DoLt, DoR}
import nsinha.{Cube, Evaluation, MinusXOrientation, MinusZOrientation, MovedCube, Moves, RubiksCube, XAxis, XOrientation, YAxis, YOrientation, ZAxis, ZOrientation}
import org.scalatest.funspec.AnyFunSpec

import scala.Console.CYAN
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class StrategyTopLayerSet extends AnyFunSpec {

  it("test top set") {
    val rubik = new RubiksCube(2)
    val listOfResets = List(MovedCube(0,1,0,1,1,0,ZOrientation,YOrientation,MinusXOrientation),
      MovedCube(1,1,0,1,1,1,ZOrientation,YOrientation,MinusXOrientation), MovedCube(0,1,1,0,1,0,XOrientation,MinusZOrientation,YOrientation),
      MovedCube(1,1,1,0,1,1,ZOrientation,YOrientation,MinusXOrientation))
    rubik.ressetCube(listOfResets)
    printDerangedCubes(rubik)
    val res = rubik.findDisarrangedCubes().headOption map { c =>
      val cubeAllFixes =  rubik.findFixForCube(c, 14).toList.map(x => x.toList)
      (c, cubeAllFixes.map(x => (x, x.foldLeft("")((z,y) => z + y.toString))))
    }


    println(res.get._2.size)


   /* val derangedCubes =  rubik.findDisarrangedCubes().foldLeft (mutable.Set[Cube]()) { (Z,c) =>
      Z += c
      Z
    }.toSet
    //so we have many cubes each has a set of moves that can correct it whithe their string rep/
    //what moves can be common among them.if their string rep matches
    val mapHashToCubes = mutable.HashMap[String, mutable.Set[Cube]]()
    val mapHashResolver = mutable.HashMap[String, List[Moves]]()
    res.foreach { x =>
      val c = x._1
      x._2.foreach { y =>
        val moves = y._1
        val hash = y._2
        if (!mapHashResolver.contains(hash)) {
          mapHashResolver(hash) = moves
        }
        if (!mapHashToCubes.contains(hash)) {
          mapHashToCubes(hash) = mutable.Set[Cube]()
        }
        mapHashToCubes(hash) += c
      }
    }

    val commonMoves = mapHashToCubes.foldLeft(mutable.Set[String]()) { (Z, entry) =>
      if (entry._2.toSet == derangedCubes) {
        Z += entry._1
      }
      Z
    }.map(x => mapHashResolver(x))

    println(commonMoves) */
  }





  it("power of moves 3 of type FRF' D2") {
    val rubik = RubiksCube(2)
    val moves = Array(DoFRFt, DoD, DoFRFt, DoDt, DoR)

    moves.foreach(_.doMove(rubik))
    rubik.findDisarrangedCubes().map(c => println(c))
    println()
    rubik.findDisarrangedByPosCubes().map(c => println(c))
    println()
    println(rubik.getMovesMap())
    printDerangedCubes(rubik)
    printDerangedPermCycles(rubik)
    println()

  }





}
