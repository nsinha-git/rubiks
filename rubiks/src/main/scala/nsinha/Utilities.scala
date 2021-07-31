package nsinha

import scala.collection.immutable.Range
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object Utilities {

  def getAllPos(n: Int) = {

    val allPos = ListBuffer[(Int, Int, Int)]()
    for (x <- Range(0, n))
      for (y <- Range(0, n))
        for (z <- Range(0, n)) {
          allPos += Tuple3(x, y, z)
        }

    allPos.toList
  }
  def mapToAxes(i:  Int) = {
    i match {
      case 1 => XAxis
      case 2 => YAxis
      case 3 => ZAxis
    }
  }

  def getAxisFromTo(rotAxis: Orientation): (Int, Int) = {
    rotAxis match {
      case XOrientation => (2, 3)
      case MinusXOrientation => (3, 2)
      case YOrientation => (3, 1)
      case MinusYOrientation => (1, 3)
      case ZOrientation => (1, 2)
      case MinusZOrientation => (2, 1)
    }
  }

  def move(oldPos: (Int, Int, Int), currAxis: Int, rotAxis: Orientation, n: Int): Int = {
    val axesFromTo = getAxisFromTo(rotAxis)
    move(oldPos, currAxis, axesFromTo._1, axesFromTo._2, n)
  }

  def move(oldPos: (Int, Int, Int), currAxis: Int, from: Int, to: Int, n: Int): Int = {
    /*
    3 -> 2 == 2->-3 == -3 ->-2 == -2 ->3:
    (x,0,0),(x,0,1),(x,0,2) -> (x,0,2), (x,1,2), (x,2,2)
    (x,1,0),(x,1,1),(x,1,2) -> (x,0,1), (x,1,1), (x,2,1)
    (x,2,0),(x,2,1),(x,2,2) -> (x,0,0), (x,1,0), (x,2,0)
    */
    if (Set(from, to, -to).contains(currAxis)) {
      if (from == currAxis) {
        if (to > 0) {
          //-to moves to from
          n - 1 - getTupleValAt(oldPos, to)
        } else {
          //to < 0 so abs(to) moves to from
          getTupleValAt(oldPos, to)
        }
      } else {
        //currAxis must be to or -to
        if (to > 0) {
          //from moves to 'to'
          getTupleValAt(oldPos, from)
        } else {
          //-from moves to to
          n - 1 - getTupleValAt(oldPos, from)
        }
      }
    } else {
      getTupleValAt(oldPos, currAxis)
    }
  }

  def getTupleValAt(tuple: (Int, Int, Int), i: Int): Int = {
    import math._
    if (abs(i) == 1) {
      tuple._1
    } else if (abs(i) == 2) {
      tuple._2
    } else {
      tuple._3
    }
  }

  def fixOrientation(cube: Cube, fromAxis: Int, toAxis: Int) = {
    cube.orientX = orient(cube.orientX, fromAxis, toAxis)
    cube.orientY = orient(cube.orientY, fromAxis, toAxis)
    cube.orientZ = orient(cube.orientZ, fromAxis, toAxis)
  }

  def orient(orig: Orientation, from: Int, to: Int): Orientation = {
    if (from == to) return orig
    (from, to) match {
      case (1, 2) => orig match {
        case XOrientation => YOrientation
        case YOrientation => MinusXOrientation
        case ZOrientation => ZOrientation
        case MinusXOrientation => MinusYOrientation
        case MinusYOrientation => XOrientation
        case MinusZOrientation => MinusZOrientation
      }
      case (1, 3) => orig match {
        case XOrientation => ZOrientation
        case YOrientation => YOrientation
        case ZOrientation => MinusXOrientation
        case MinusXOrientation => MinusZOrientation
        case MinusYOrientation => MinusYOrientation
        case MinusZOrientation => XOrientation
      }
      case (2, 1) => orig match {
        case XOrientation => MinusYOrientation
        case YOrientation => XOrientation
        case ZOrientation => ZOrientation
        case MinusXOrientation => YOrientation
        case MinusYOrientation => MinusXOrientation
        case MinusZOrientation => MinusZOrientation
      }
      case (2, 3) => orig match {
        case XOrientation => XOrientation
        case YOrientation => ZOrientation
        case ZOrientation => MinusYOrientation
        case MinusXOrientation => MinusXOrientation
        case MinusYOrientation => MinusZOrientation
        case MinusZOrientation => YOrientation
      }
      case (3, 1) => orig match {
        case XOrientation => MinusZOrientation
        case YOrientation => YOrientation
        case ZOrientation => XOrientation
        case MinusXOrientation => ZOrientation
        case MinusYOrientation => MinusYOrientation
        case MinusZOrientation => MinusXOrientation
      }
      case (3, 2) => orig match {
        case XOrientation => XOrientation
        case YOrientation => MinusZOrientation
        case ZOrientation => YOrientation
        case MinusXOrientation => MinusXOrientation
        case MinusYOrientation => ZOrientation
        case MinusZOrientation => MinusYOrientation
      }
    }
  }

  def isCubeDeranged(c: Cube): Boolean = {
    c.orientX != XOrientation || c.orientY != YOrientation ||
      c.orientZ != ZOrientation || c.currX != c.origX ||
      c.currY != c.origY || c.currZ != c.origZ
  }

  def isCubeDislocated(c: Cube): Boolean = {
      c.currX != c.origX || c.currY != c.origY || c.currZ != c.origZ
  }

  def printDerangedCubes(r: RubiksCube): Unit = {
    val n = r.n
    val deranged = r.findDisarrangedCubes().toSet
    val orderingzyx = new Ordering[(Int, Int, Int)]() {
      override def compare(a: (Int, Int, Int), b: (Int, Int, Int)): Int = {
        if (a._1 < b._1) return -1
        if (a._2 < b._2) return -1
        if (a._3 < b._3) return -1
        if (a._1 > b._1) return 1
        if (a._2 > b._2) return 1
        if (a._3 > b._3) return 1
        0
      }
    }
    val que = mutable.ListBuffer[String]()
    //create n^2 slices at z axis and print the pos.
    for (i <- Range(0, n)) {
      val listOfLocs = getSlice(3, i, n).sorted(orderingzyx)
      listOfLocs.foreach { x =>
        val cube = r.getCubeAtLoc(x)
        val printDeranged = () => {
          if (deranged.contains(cube)) s"${cube.origX}/${cube.currX}, ${cube.origY}/${cube.currY}, ${cube.origZ}/${cube.currZ} " else ".             "
        }
        que += (s"${x}: ${printDeranged()}")
      }
    }

    var i = 0
    while (i < que.size) {
      val el = que(i)
      if (i % n == 0) {
        println()
      }
      if (i % (n * n) == 0) {
        println()
      }
      print(el)
      i += 1
    }
    println()

  }

  def printDerangedPermCycles(rubiks: RubiksCube): Set[Set[(Int, Int, Int)]]= {
    val cubes: Set[Cube] = rubiks.findDisarrangedCubes().toSet
    val res = mutable.ListBuffer[mutable.Set[(Int, Int, Int)]]()
    val que = cubes.toList
    for (i <- Range(0, que.size)) {
      val cube = que(i)
      val t = res.forall(x => !x.contains((cube.origX, cube.origY, cube.origZ)))
      val q = res.forall(x => !x.contains((cube.currX, cube.currY, cube.currZ)))
      if (t & q) { //none conatins this cube or its curr pos
        val newEntry = mutable.Set[(Int, Int, Int)]()
        newEntry += ((cube.origX, cube.origY, cube.origZ))
        newEntry += ((cube.currX, cube.currY, cube.currZ))
        res += newEntry
      } else { //one of them contains entry.
        res.foreach { x =>
          if (x.contains((cube.origX, cube.origY, cube.origZ))) {
            x += ((cube.currX, cube.currY, cube.currZ))
          } else if (x.contains((cube.currX, cube.currY, cube.currZ))) {
            x += ((cube.origX, cube.origY, cube.origZ))
          } else {
            //this x does not contain cube or its current pos. ignore
          }
        }
      }
    }
    //merge res entries if they have any common
    for (i <- Range(0,res.size)) {
      val currSet = res(i)
      for (j <- Range(i+1, res.size)) {
        if (res(j).intersect(currSet).nonEmpty) {
          res(j).foreach(y => currSet += y)
          res(j).clear()
        }
      }
    }
    val res1 = res.filter(x => x.nonEmpty).map(x => x.toSet).toSet
    println("PERM CYCLES <")
    res1.foreach(println(_))
    println("PERM CYCLES/>")
    res1

  }



  def getSlice(axis: Int, idxAxis: Int, n: Int) = {
    //n^2 cubes in motion
    getAllPos(n).filter(x => getTupleValAt(x, axis) == idxAxis)
  }

  def frequencyChartMoves(movesLists: List[List[List[Orientation]]]) = {
    val frqMap = new mutable.HashMap[Orientation, Int]()
    for (moveList <- movesLists) {
      for (move <- moveList) {
        if (frqMap.contains(move.head)) {
          frqMap(move.head) = frqMap(move.head) + 1
        } else {
          frqMap(move.head) = 1
        }
        if (move.size > 100) {
          val secondElem = move(2)
          if (frqMap.contains(secondElem)) {
            frqMap(secondElem) = frqMap(secondElem) + 2
          } else {
            frqMap(secondElem) = 2
          }
        }
      }
    }
    frqMap.toMap
  }

  def getRandomMove(): Moves = {
    val rand = new Random(System.nanoTime())
    Moves(mapToAxes(rand.nextInt(3)), mapToAxes(rand.nextInt(3)))
  }

  def getRandomCubePos(n: Int): (Int, Int, Int) = {
    val rand = new Random(System.nanoTime())
    (rand.nextInt(3), rand.nextInt(3), rand.nextInt(3))
  }

  def getTopFrequenciesMove(cubeMovs: Map[Cube, List[List[Orientation]]], freqMap: Map[Orientation, Int]): List[(Cube, Orientation)] = {
    val maxFreq = freqMap.values.max
    val limit = math.max(maxFreq/2 + 1, maxFreq - 2 )
    val orientationsToConsider = freqMap.toList.filter{case (x,y) => y >= limit} map (_._1)

    orientationsToConsider map { or =>
      cubeMovs.find {case (c,l) => l.map(x => x.head).toSet.contains(or)}.get._1 -> or
    }
  }

  def getFreqDistMoves(rubik:RubiksCube) = {
    val cubeMoves = rubik.findDisarrangedCubes().map({ x =>
      val derangement = Derangement.getDerangementOfCube(x, 3)
      val moves = Derangement.getMovesForOrientation(derangement)
      println(x)
      moves.foreach(println(_))
      println()
      x -> moves
    }).toMap

    (cubeMoves, frequencyChartMoves(cubeMoves.values.toList))
  }



}
