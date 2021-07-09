package nsinha

import nsinha.Utilities.move

import scala.::
import scala.collection.mutable.ListBuffer

case class Derangement(xOrient: Orientation, yOrient: Orientation, zOrient: Orientation, xPos: (Int, Int), yPos: (Int, Int), zPos: (Int, Int), n:Int)


object Derangement {
  def getDerangementOfCube(c: Cube, n: Int): Derangement = {
    Derangement(c.orientX, c.orientY, c.orientZ, (c.origX, c.currX), (c.origY, c.currY), (c.origZ, c.currZ), n)
  }

  def getMovesForOrientation(derangement: Derangement) = {
    val rots = Set(orientCorrection(XOrientation, derangement.xOrient), orientCorrection(YOrientation, derangement.yOrient),
      orientCorrection(ZOrientation, derangement.zOrient)).filter( _ != None).map (_.get)
    val combs = ListBuffer[List[Orientation]]()
    val n = derangement.n

    for (movesSeq <- genAllCombs(rots)) {
      //movesSeq is one way of coorecting the orientation.
      //will it correct the pos. if yes keep it. else throw it
      var pos =  (derangement.xPos._2, derangement.yPos._2,  derangement.zPos._2)
      for (rot <- movesSeq) {
        pos = (move(pos,1, rot, n), move(pos,2, rot, n),
          move(pos,3, rot, n))
      }
       if (pos._1 == derangement.xPos._1 && pos._2 == derangement.yPos._1 && pos._3 == derangement.zPos._1) {
         combs += movesSeq
       }
    }
  }

  def genAllCombs(set: Set[Orientation]): List[List[Orientation]] = {
    if (set.isEmpty) {
      List()
    } else {
      val combs = ListBuffer[List[Orientation]]()
      set.map { s =>
        genAllCombs(set.diff(Set(s))) map (li => combs += (li.::(s)))
      }
      combs.toList
    }
  }

  //axial move
  def orientCorrection(a: Orientation, b: Orientation): Option[Orientation] = {
    if (a == b) return None
     val res = (a, b) match {
       case (XOrientation, YOrientation) => ZOrientation
       case (XOrientation, MinusYOrientation) => MinusZOrientation
       case (XOrientation, ZOrientation) => YOrientation
       case (XOrientation, MinusZOrientation) => MinusYOrientation
       case (YOrientation, XOrientation) => MinusZOrientation
       case (YOrientation, MinusXOrientation) => ZOrientation
       case (YOrientation, ZOrientation) => MinusXOrientation
       case (YOrientation, MinusZOrientation) => XOrientation
       case (ZOrientation, XOrientation) => MinusYOrientation
       case (ZOrientation, MinusXOrientation) => YOrientation
       case (ZOrientation, YOrientation) => XOrientation
       case (ZOrientation, MinusYOrientation) => MinusXOrientation
     }
    Option(res)
  }

  /**
   * (x-y) (x-y):
   * (1-2,-1->-2,2->-1,-2->1)
   * so(1->-1, -1->1,2->-2, -2->2)
   *
   * is it a plane/ or two planes
   * if plane then can.Move = 1
   * else there are 3 moves x-axis,y-axis, z-axis..
   * e.g 2 cubes have 3 first moves available, 2 cubes have only one -move on Z axis, remaining two cubes have x-axis moves.
   * none have a y-axis move.
   * In this case move 2 cubes by a z axis/x axis.
   * evaluate both. in z axis move:two z axis cubes are connnected.At completion the 4 cubes have x axis moves.
   * in x axis move on cube 1: complexity is not reduced.
   *
   * Other way is phasors. Phasor of cube at corners(8). phasor of middle cubes(6) and phasor of in-between cubes(12). center cube(1).
   * corner -cubes: 2 require a x move, 2 y move, 2 are ok, 2 require y move then x move.
   *
   * one more way:3 cubes require y then x moves or
   *
   * a simple idea is to just print the derangement of cubes and make move by human mind.
   * This tool can be used to discover best moves given the condition of derangement
   *
   *
   *
   *
   */
}
