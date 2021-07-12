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

  def move(oldPos: (Int, Int, Int), currAxis: Int, rotAxis: Orientation, n: Int): Int = {
    rotAxis match {
      case XOrientation => move(oldPos, currAxis, 2, 3, n)
      case MinusXOrientation => move(oldPos, currAxis, 3, 2, n)
      case YOrientation => move(oldPos, currAxis, 3, 1, n)
      case MinusYOrientation => move(oldPos, currAxis, 1, 3, n)
      case ZOrientation => move(oldPos, currAxis, 1, 2, n)
      case MinusZOrientation => move(oldPos, currAxis, 2, 1, n)
    }
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

  def printDerangedCubes(r: RubiksCube): Unit = {
    val n = r.n
    val deranged = r.findDisarrangedCubes().toSet
    val orderingzyx = new Ordering[(Int, Int, Int)]() {
      override def compare(x: (Int, Int, Int), y: (Int, Int, Int)): Int = {
        if (x._3 < y._3) return -1
        if (x._3 > y._3) return 1
        if (x._2 < y._2) return -1
        if (x._2 > y._2) return 1
        if (x._1 < y._1) return -1
        if (x._1 > y._1) return 1
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

  def getSlice(axis: Int, value: Int, n: Int) = {
    //n^2 cubes in motion
    getAllPos(n).filter(x => getTupleValAt(x, axis) == value)
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
      }
    }
    frqMap.toMap
  }

  def getRandomMove(): Moves = {
    val rand = new Random(System.nanoTime())
    Moves(intToOrientation(rand.nextInt(3)), intToOrientation(rand.nextInt(3)))
  }

  def getRandomCubePos(n: Int): (Int, Int, Int) = {
    val rand = new Random(System.nanoTime())
    (rand.nextInt(3), rand.nextInt(3), rand.nextInt(3))
  }

  def intToOrientation(i: Int): Axis = {
    i match {
      case 0 => XAxis
      case 1 => YAxis
      case 2 => ZAxis
    }
  }


}
