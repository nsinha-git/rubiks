package nsinha

import nsinha.Axis.{convertAxisToCordinate, findUniqueOne}
import nsinha.Utilities.{fixOrientation, getTupleValAt, move}

import scala.collection.immutable.Range
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class RubiksCube (n: Int) {
  private val cubes = ListBuffer[Cube]()
  private val posMap = mutable.HashMap[(Int, Int, Int), Cube]()
  private val revPosMap = mutable.HashMap[Cube, (Int, Int, Int)]()
  private val allPos = {
    val _allPos = ListBuffer[(Int, Int, Int)]()
    for (x <- Range(0, n))
      for (y <- Range(0, n))
        for (z <- Range(0, n)) {
          _allPos += Tuple3(x,y,z)
        }

    _allPos.toList
  }

  createRubix()


  //n rows and n cols and n depths  and 6 directions. n*n*n cuboids
  //every cube has a original pos, and orientation.
  //any move can take a slice and transpose two dirs i.e x->y->-x->-y->x

  private def createRubix(): Unit = {
    for (x <- Range(0, n))
      for (y <- Range(0, n))
        for (z <- Range(0, n)) {
          val cube = Cube(x,y,z,x,y,z)
          cubes += cube
          posMap((x,y,z)) = cube
          revPosMap(cube) = (x,y,z)
        }
  }

  /**
   * given a point in (x,y,z) and the fromAxis toAxis, make a move of a slice.
   * @param fromAxis
   * @param toAxis
   * @param point
   */

  def makeMove(fromAxis: Axis, toAxis: Axis, point: (Int, Int, Int)): Unit = {
    import Axis._
    val from = convertAxisToCordinate(fromAxis)
    val to = convertAxisToCordinate(toAxis)
    val unchangedAxis = findUniqueZero(or(from, to))
    val unchangedAxisCord = getCoordOfPoint(point, unchangedAxis)
    val posAffected = getSlice(unchangedAxis, unchangedAxisCord)
    doMove(posAffected, unchangedAxis, from, to)


  }

  def getSlice(axis: Int, value: Int) = {
    //n^2 cubes in motion
    allPos.filter(x => getTupleValAt(x, axis) == value)
  }

  /**
   * move all pos in slice to based on from to
   * @param slicePos
   * @param unchangedAxis
   * @param from
   * @param to
   */
  def doMove(slicePos: List[(Int, Int, Int)], unchangedAxis: Int, from: (Int, Int, Int), to: (Int, Int, Int)) = {
    import Axis._
    slicePos.map(pos => posMap(pos)).foreach(cube => movePos(cube, findUniqueOne(from), findUniqueOne(to)))
  }

  /**
   * just move an oldPos from fromAxis to ToAxis and dir
   * @param oldPos
   * @param fromAxis like 1, 2, 3
   * @param toAxis  like 1, 2, 3 or negatives
   * @param dir
   */
  def movePos(cube: Cube, fromAxis: Int, toAxis: Int) = {
    val newPos = adjustCubePosAndOrientation(cube, fromAxis, toAxis)
    setCubeToNewPosOnRubiks(cube, newPos)
  }


  def adjustCubePosAndOrientation(cube: Cube, fromAxis: Int, toAxis: Int): (Int, Int, Int) = {
    val oldPos = (cube.currX, cube.currY, cube.currZ)
    val newPos = (move(oldPos,1, fromAxis, toAxis, n), move(oldPos,2, fromAxis, toAxis, n),
      move(oldPos,3, fromAxis, toAxis, n))
    cube.currX = newPos._1
    cube.currY = newPos._2
    cube.currZ = newPos._3
    fixOrientation(cube, fromAxis, toAxis)
    newPos
  }

  /**
   * DANGEROUS
   * This affects the state of rubiks.
   * As per assumption the caller will make sure something will move to that location.
   * This code will mark the oldPos null. At end of this its not a guarantee that global state
   * is correct as there can be two cubes at same location!!
   */
  def setCubeToNewPosOnRubiks(cube: Cube, newPos: (Int, Int, Int)): Unit = {
    val oldPos = (cube.currX, cube.currY, cube.currZ)
    posMap(newPos) = cube
    revPosMap(cube) = newPos
    posMap(oldPos) = null

  }

  /**
   * find cubes not at correct location or orientation
   * @return
   */
  def findDisarrangedCubes(): List[Cube] = {
    cubes.filter(isCubeDeranged(_)).toList
  }

  def isCubeDeranged(c: Cube): Boolean = {
    c.orientX != XOrientation || c.orientY != YOrientation ||
      c.orientZ != ZOrientation || c.currX != c.origX ||
      c.currY != c.origY || c.currZ != c.origZ
  }


  /**
   * given deranged cube. find the min moves reqd to correct it.
   * e.g 0,0,0 is at 2,0,0 then then a min move is x-Z or x->Y.
   * based on orientation it might be x->Z
   */
  def findFixForCube(cube: Cube, maxDepth: Int = 3, pathTillNow :ListBuffer[Moves] = new ListBuffer[Moves]()): mutable.ListBuffer[mutable.ListBuffer[Moves]]= {
    //do a dfs search, input is current path, o/p can be multiple forks.
    //one slice rotation can cover a difference o n, most can be captured by depth of 4.
    //never use same move 3 times in conhunction as its possible to do an opposite move to get same effect
    if (!isCubeDeranged(cube) || maxDepth <= 0) return ListBuffer(pathTillNow)
    val lastMoveOptional = pathTillNow.lastOption
    val allPathsFromHere = mutable.ListBuffer[mutable.ListBuffer[Moves]]()

    for (move <- Moves.allMoves() if !Moves.oppositeMoves(lastMoveOptional, move)) {
      val cubeCopy = cube.copy()
      val moveList = ListBuffer(move)
      adjustCubePosAndOrientation(cubeCopy, findUniqueOne(convertAxisToCordinate(move.a)), findUniqueOne(convertAxisToCordinate(move.b)))
      //cubeCopy is now moved. lets do dfs on this
      val currlistOfPaths = findFixForCube(cubeCopy, maxDepth - 1, moveList)
      currlistOfPaths.foreach(p => allPathsFromHere += p)
    }
    allPathsFromHere map {x =>
      val t = pathTillNow.toList ++ x.toList
      val res = new ListBuffer[Moves]()
      t foreach(x => res += x)
      res
    }
  }

  /**
   *
    * @param mapOfCubeFixes
   * @return
   */
  def findMostCommonFix(mapOfCubeFixes: Map[Cube, List[List[Moves]]]): Option[Moves] = {
    var moveUnion = MoveUnion(null)
    mapOfCubeFixes.foreach { x =>
      val cube = x._1
      val listOfPathsForCube = x._2
      val firstMovesForThisCube = MoveUnion(x._2.map(y => y.head))
      moveUnion = MoveUnion.intersect(moveUnion, firstMovesForThisCube)
    }
    if (moveUnion.moves.isEmpty) {
      None
    } else {
      Option(moveUnion.moves.head)
    }
  }
}
