package org.nsinha
package quadtrees

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * store the 2d points.
 * Given a point(may be not in stored list) and a distance r find nbrs within that distance.
 * A leaf node  contains one point. its split whenever it has to keep more than one point
 *
 */

class QuadTreeNode(_t: (Int, Int)) {
  import QuadTreeImpl._
  var holdsPoint = false
  var insertionPoint: (Int,Int) = _t
  val listKids: Array[QuadTreeNode] = Array(null,null,null,null)

  def insertPoint(x: (Int, Int)): Unit = {
    if (x == insertionPoint) {
      holdsPoint = true
    } else {
      val quad = getRelativeQuadrant(x, insertionPoint)
      var nextQuadTreeNode = listKids(quad)
      if (nextQuadTreeNode == null) {
        nextQuadTreeNode = new QuadTreeNode(x)
        listKids(quad) = nextQuadTreeNode
      }
      nextQuadTreeNode.insertPoint(x)
    }

  }

  def pointExists(x:(Int, Int)): Boolean = {
    if (x == insertionPoint) {
      true
    } else {
      val quad = getRelativeQuadrant(x, insertionPoint)
      var nextQuadTreeNode = listKids(quad)
      if (nextQuadTreeNode == null) {
        false
      } else {
        nextQuadTreeNode.pointExists(x)
      }
    }
  }



  def getPointsInBox(x:(Int, Int), y: (Int, Int), res:  ListBuffer[(Int, Int)]): Unit = {
    /**
     * Algo. a bound box and quad node
     */
    val isInsertionPointInsideBox = isPointInsideBoundingBox(insertionPoint, x, y)
    if (isInsertionPointInsideBox) {
      if (holdsPoint) {
        res += insertionPoint
      }
      //traverse in all directions
      for (l <- listKids if l != null) {
        l.getPointsInBox(x,y,res)
      }
    } else {
      //ip not in bounding box
      val q1 = getRelativeQuadrant(x, insertionPoint)
      val q2 = getRelativeQuadrant(y, insertionPoint)
      for (q <- Set(q1, q2)) {
        if (listKids(q) != null) {
          listKids(q).getPointsInBox(x,y,res)
        }
      }
    }

  }

  def getPointsInBoxSlopeHigherThan(x:(Int, Int), y: (Int, Int), slope: (Int, Int), res:  ListBuffer[(Int, Int)]): Unit = {
    val isInsertionPointInsideBoxAndWithinSlope = isPointInsideBoundingBoxAndHigherSlope(insertionPoint, x, y, slope)
    if (isInsertionPointInsideBoxAndWithinSlope) {
      if (holdsPoint) {
        res += insertionPoint
      }
      //traverse in all directions
      for (l <- listKids if l != null) {
        l.getPointsInBoxSlopeHigherThan(x,y, slope, res)
      }
    } else {
      //ip not in bounding box
      val q1 = getRelativeQuadrant(x, insertionPoint)
      val q2 = getRelativeQuadrant(y, insertionPoint)
      for (q <- Set(q1, q2)) {
        if (listKids(q) != null) {
          listKids(q).getPointsInBoxSlopeHigherThan(x,y,slope, res)
        }
      }
    }
  }

  def getPointsInBoxSlopeLowerThan(x:(Int, Int), y: (Int, Int), slope: (Int, Int), res:  ListBuffer[(Int, Int)]): Unit = {
    val isInsertionPointInsideBoxAndWithinSlope = isPointInsideBoundingBoxAndLowerSlope(insertionPoint, x, y, slope)
    if (isInsertionPointInsideBoxAndWithinSlope) {
      if (holdsPoint) {
        res += insertionPoint
      }
      //traverse in all directions
      for (l <- listKids if l != null) {
        l.getPointsInBoxSlopeLowerThan(x,y, slope, res)
      }
    } else {
      //ip not in bounding box
      val q1 = getRelativeQuadrant(x, insertionPoint)
      val q2 = getRelativeQuadrant(y, insertionPoint)
      for (q <- Set(q1, q2)) {
        if (listKids(q) != null) {
          listKids(q).getPointsInBoxSlopeLowerThan(x,y,slope, res)
        }
      }
    }
  }


  def isPointInsideBoundingBox(p: (Int, Int), x:(Int, Int), y: (Int, Int)) = {
    if ((p._1 >= x._1 && p._2 >= x._2)  && (p._1 < y._1 && p._2 < y._2)) {
      true
    } else {
      false
    }
  }


  def isPointInsideBoundingBoxAndHigherSlope(p: (Int, Int), x:(Int, Int), y: (Int, Int), slope: (Int, Int)) = {
    val within = if ((p._1 >= x._1 && p._2 >= x._2)  && (p._1 < y._1 && p._2 < y._2)) {
      true
    } else {
      false
    }
    if (within) {
      val yt = p._2 - x._2
      val xt = p._1 - x._1
      if (yt * slope._2 > xt * slope._1) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }


  def isPointInsideBoundingBoxAndLowerSlope(p: (Int, Int), x:(Int, Int), y: (Int, Int), slope: (Int, Int)) = {
    val within = if ((p._1 >= x._1 && p._2 >= x._2)  && (p._1 < y._1 && p._2 < y._2)) {
      true
    } else {
      false
    }
    if (within) {
      val yt = p._2 - x._2
      val xt = p._1 - x._1
      if (yt * slope._2 > xt * slope._1) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }

}
class QuadTreeImpl(_pts: List[(Int, Int)] = null){
  var rootNode: QuadTreeNode = _

  if (_pts != null) {
    _pts.foreach(insertPoint(_))
  }

  val pts = ListBuffer[(Int, Int)]()


  def insertPoint(x: (Int, Int)): Unit = {
    if (rootNode == null) {
      rootNode = new QuadTreeNode(x)
    }
    rootNode.insertPoint(x)
    pts += x
  }

  def pointExists(x:(Int, Int)): Boolean = {
    rootNode.pointExists(x)
  }

  def getPointsInBox(x:(Int, Int), y: (Int, Int)): List[(Int, Int)] = {
    val res = ListBuffer[(Int, Int)]()
    rootNode.getPointsInBox(x, y, res)
    res.toList
  }

  def getPointsInBox(pts:((Int, Int),(Int, Int))): List[(Int, Int)] = {
    getPointsInBox(pts._1, pts._2)
  }

  def getAllPoints() = {
    pts
  }

  def getPointsInBoxSlopeHigherThan(x:(Int, Int), y: (Int, Int)) = {
    val slope = (y._2 - x._2, y._1 - x._1)
    val res = ListBuffer[(Int, Int)]()
    rootNode.getPointsInBoxSlopeHigherThan(x, y, slope, res)
    res.toList
  }

  def getPointsInBoxSlopeLowerThan(x:(Int, Int), y: (Int, Int)) = {
    val slope = (y._2 - x._2, y._1 - x._1)
    val res = ListBuffer[(Int, Int)]()
    rootNode.getPointsInBoxSlopeLowerThan(x, y, slope, res)
    res.toList
  }
}

object QuadTreeImpl {
  def generatePoint(): (Int, Int) = {
    val x = Random.between(1, 99)
    val y = Random.between(1, 99)
    (x,y)
  }

  def generatePoints(): List[(Int, Int)] = {
    val res = ListBuffer[(Int, Int)]()
    for ( i <- Range(1, 10000)) {
      res += generatePoint()
    }
    res.toList

  }
  def getRelativeQuadrant(x: (Int, Int), y: (Int, Int)): Int = {
    if (x._1 >= y._1 && x._2 >= y._2) {
      0
    } else if(x._1 < y._1 && x._2 < y._2) {
      2
    } else if (x._1 > y._1) {
      3
    } else {
      1
    }
  }



  def main(args: Array[String]): Unit = {
    val points = generatePoints()
    val quadTreeImpl = new QuadTreeImpl()
    for (p <- points) {
      quadTreeImpl.insertPoint(p)
    }
    assert(points.forall(quadTreeImpl.pointExists(_)))
    val pts = quadTreeImpl.getPointsInBox((0,0), (100,100))
    println(pts.size)
    println(points.size)
    println((points.toSet.size))
    //pts.foreach(println(_))

  }
}
