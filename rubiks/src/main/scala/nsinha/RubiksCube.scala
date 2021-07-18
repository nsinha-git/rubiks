package nsinha

import nsinha.Axis.{convertAxisToCordinate, findUniqueOne}
import Utilities._

import scala.Console.{GREEN, RESET}
import scala.collection.immutable.Range
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class RubiksCube(n: Int) {
  private val cubes = ListBuffer[Cube]()
  private[nsinha] val posMap = mutable.HashMap[(Int, Int, Int), Cube]()
  private[nsinha] val revPosMap = mutable.HashMap[Cube, (Int, Int, Int)]()
  val allPos = getAllPos(n)

  createRubix()

  //make a copy of this instance
  def copy(): RubiksCube = {
    val cp = RubiksCube(n)
    for ((c, c1) <- cubes.zip(cp.cubes)) {
      assert(c1.origX == c.origX && c1.origY == c.origY && c1.origZ == c.origZ)
      c1.currX = c.currX
      c1.currY = c.currY
      c1.currZ = c.currZ
      c1.orientX = c.orientX
      c1.orientY = c.orientY
      c1.orientZ = c.orientZ
      cp.posMap((c1.currX, c1.currY, c1.currZ)) = c1
      cp.revPosMap(c1) = (c1.currX, c1.currY, c1.currZ)
    }
    cp
  }


  //n rows and n cols and n depths  and 6 directions. n*n*n cuboids
  //every cube has a original pos, and orientation.
  //any move can take a slice and transpose two dirs i.e x->y->-x->-y->x

  private def createRubix(): Unit = {
    for (x <- Range(0, n))
      for (y <- Range(0, n))
        for (z <- Range(0, n)) {
          val cube = Cube(x, y, z, x, y, z)
          cubes += cube
          posMap((x, y, z)) = cube
          revPosMap(cube) = (x, y, z)
        }
  }

  /**
   * given a point in (x,y,z) and the fromAxis toAxis, make a move of a slice.
   *
   * @param fromAxis
   * @param toAxis
   * @param point
   */

  def makeMove(fromAxis: Axis, toAxis: Axis, point: (Int, Int, Int)): Unit = {
    import nsinha.Axis._
    val from = convertAxisToCordinate(fromAxis)
    val to = convertAxisToCordinate(toAxis)
    val unchangedAxis = findUniqueZero(or(from, to))
    val unchangedAxisCord = getCoordOfPoint(point, unchangedAxis)
    val posAffected = getSlice(unchangedAxis, unchangedAxisCord, n)
    doMove(posAffected, unchangedAxis, from, to)


  }

  def getCubeAtLoc(pos: (Int, Int, Int)): Cube = {
    posMap(pos)
  }

  /**
   * move all pos in slice to based on from to
   *
   * @param slicePos
   * @param unchangedAxis
   * @param from
   * @param to
   */
  def doMove(slicePos: List[(Int, Int, Int)], unchangedAxis: Int, from: (Int, Int, Int), to: (Int, Int, Int)) = {
    import nsinha.Axis._
    val scratchPosMap = mutable.HashMap[(Int, Int, Int), Cube]()
    slicePos.map(pos => posMap(pos)).foreach(cube => movePos(cube, findUniqueOne(from), findUniqueOne(to), scratchPosMap))
    //scratchPosMap should be applied now to posMap and revPosMap
    applyScratchPosMap(scratchPosMap)
  }

  def applyScratchPosMap(scratchPosMap: mutable.HashMap[(Int, Int, Int), Cube]) = {
    for ((pos, cube) <- scratchPosMap) {
      posMap(pos) = cube //replaces old one. but the previous pos of cube is not updated. This will happen subsequently
      revPosMap(cube) = pos
    }
  }

  /**
   * just move an oldPos from fromAxis to ToAxis and dir
   *
   * @param oldPos
   * @param fromAxis like 1, 2, 3
   * @param toAxis   like 1, 2, 3 or negatives
   * @param dir
   */
  def movePos(cube: Cube, fromAxis: Int, toAxis: Int, scratchPosMap: mutable.HashMap[(Int, Int, Int), Cube]) = {
    val newPos = adjustCubePosAndOrientation(cube, fromAxis, toAxis)
    setCubeToNewPosOnRubiks(cube, newPos, scratchPosMap)
  }

  /**
   *
   * @param cube
   * @param fromAxis positive
   * @param toAxis   positive
   * @return
   */
  def adjustCubePosAndOrientation(cube: Cube, fromAxis: Int, toAxis: Int): (Int, Int, Int) = {
    val oldPos = (cube.currX, cube.currY, cube.currZ)
    val newPos = (move(oldPos, 1, fromAxis, toAxis, n), move(oldPos, 2, fromAxis, toAxis, n),
      move(oldPos, 3, fromAxis, toAxis, n))
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
  def setCubeToNewPosOnRubiks(cube: Cube, newPos: (Int, Int, Int), scratchPosMap: mutable.HashMap[(Int, Int, Int), Cube]): Unit = {
    val oldPos = (cube.currX, cube.currY, cube.currZ)
    scratchPosMap(newPos) = cube
  }

  /**
   * find cubes not at correct location or orientation
   *
   * @return
   */
  def findDisarrangedCubes(): List[Cube] = {
    cubes.filter(isCubeDeranged(_)).toList
  }

  /**
   * given deranged cube. find the min moves reqd to correct it.
   * e.g 0,0,0 is at 2,0,0 then then a min move is x-Z or x->Y.
   * based on orientation it might be x->Z
   */
  def findFixForCube(cube: Cube, maxDepth: Int = 6, pathTillNow: ListBuffer[Moves] = new ListBuffer[Moves]()): mutable.ListBuffer[mutable.ListBuffer[Moves]] = {
    //do a dfs search, input is current path, o/p can be multiple forks.
    //one slice rotation can cover a difference o n, most can be captured by depth of 4.
    //never use same move 3 times in conjunction as its possible to do an opposite move to get same effect
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
    allPathsFromHere map { x =>
      val t = pathTillNow.toList ++ x.toList
      val res = new ListBuffer[Moves]()
      t foreach (x => res += x)
      res
    }
  }

  /**
   * second algo
   *
   * @param cube
   * @param maxDepth
   * @param pathTillNow
   * @return
   */
  def findFixForCube2(cube: Cube, maxDepth: Int = 6, pathTillNow: ListBuffer[Moves] = new ListBuffer[Moves]()): mutable.ListBuffer[mutable.ListBuffer[Moves]] = {
    /** this cube is dioriented
     * 1. X-Y will move Z-Z, x-y and y-> -x etc are possible moves @link Moves.getAllMoves
     * 2. Same plane moves for space correction requires at most two moves for space correction and atleast one move.
     * 3. 2 plane moves where all x,y,z are different requires at most  3 moves for space correction around different
     * axis and atleast two moves
     * 3.
     * */

    null
  }

  /**
   *
   * @param mapOfCubeFixes
   * @return
   */
  def findMostCommonFix(mapOfCubeFixes: Map[Cube, List[List[Moves]]]): Option[(Moves, Cube)] = {
    val mapOfCubeFixesLowestLen = getLowestCntPathsFromPaths(mapOfCubeFixes)
    val mapOfCubeFixesLowestLenMaxed = getLowestCntPathsFromPathsMaxed(mapOfCubeFixesLowestLen)

    var moveUnion = MoveUnion(null)
    mapOfCubeFixesLowestLenMaxed.foreach { x =>
      val firstMovesForThisCube = MoveUnion(x._2.map(y => y.head))
      moveUnion = MoveUnion.intersect(moveUnion, firstMovesForThisCube)
    }
    if (moveUnion.moves.isEmpty) {
      None
    } else {
      val pos = mapOfCubeFixesLowestLenMaxed.map(x => ((x._1.origX, x._1.origY, x._1.origZ), (x._1.currX, x._1.currY, x._1.currZ)))
      println(s"moves.size = ${moveUnion.moves.size}: ${moveUnion.moves} ")
      println("deranged cubes:")
      pos.foreach(x => println(s"""${RESET}${GREEN}${x}${RESET}"""))
      Option(moveUnion.moves.head, mapOfCubeFixesLowestLenMaxed.keySet.head)
    }
  }

  def getLowestCntPathsFromPaths(mapOfCubeFixes: Map[Cube, List[List[Moves]]]) = {
    val cntMap = new mutable.HashMap[Cube, mutable.Set[Int]]()
    val cntWisemap = new mutable.HashMap[(Cube, Int), ListBuffer[List[Moves]]]()
    val mapOfCubeFixesLowestLen = new mutable.HashMap[Cube, List[List[Moves]]]()

    for (cube <- mapOfCubeFixes.keys) {
      cntMap(cube) = new mutable.TreeSet[Int]()
      val cubeFixPaths = mapOfCubeFixes(cube).map(x => (x.size, x)).foreach { x =>
        cntMap(cube) += x._1
      }
      cntMap(cube).foreach(x => cntWisemap((cube, x)) = ListBuffer[List[Moves]]())
    }

    //we have for every cube a map of paths faceted on its length. We only neeed to consider lowest for now
    for (cube <- mapOfCubeFixes.keys) {
      mapOfCubeFixesLowestLen(cube) = mapOfCubeFixes(cube).filter(_.size == cntMap(cube).head)
    }

    mapOfCubeFixesLowestLen
  }

  /**
   * get the max len cubes as we will try to fix those before fixing lower ones
   *
   * @param mapOfCubeFixesLowestLen
   * @return
   */
  def getLowestCntPathsFromPathsMaxed(mapOfCubeFixesLowestLen: mutable.HashMap[Cube, List[List[Moves]]]) = {
    val mapOfCubeFixesLowestLenMaxed = new mutable.HashMap[Cube, List[List[Moves]]]()
    val maxLen = mapOfCubeFixesLowestLen.map(x => x._2.map(y => y.size).max).toList.max
    mapOfCubeFixesLowestLen foreach { case (c, paths) =>
      if (!paths.isEmpty) {
        if (paths.head.size == maxLen) {
          mapOfCubeFixesLowestLenMaxed(c) = paths
        }
      }
    }

    mapOfCubeFixesLowestLenMaxed
  }



  def ressetCube(movedCubes: List[MovedCube]) = {
     val t = movedCubes map { x =>
      val c = posMap(x.origPos)
      c.currX = x.newX
      c.currY = x.newY
      c.currZ = x.newZ
      c.orientX = x.orientX
      c.orientY = x.orientY
      c.orientZ = x.orientZ
       () => {
         posMap(x.newPos) = c
         revPosMap(c) = x.newPos
       }

    }
    t.foreach(_())
  }
}


case class MovedCube(origX: Int, origY: Int, origZ: Int, newX: Int, newY: Int, newZ: Int,
                     orientX: Orientation, orientY: Orientation, orientZ: Orientation) {
  def origPos = (origX, origY, origZ)
  def newPos = (newX, newY, newZ)
  def newOrientation = (orientX, orientY, orientZ)
}
