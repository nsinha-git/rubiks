package org.nsinha
package twodalgos

import org.nsinha.quadtrees.QuadTreeImpl

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * given a lot of points a convex hulll is a envelope that contains all the points inside
 *
 * i.e a point M when lined with a p in hull will meet the hull again on other side of M and p
 *
 */

/**
 *algo1:
 * for nd: (x,y)
 * 1.find max  and mins on each axis.all points in ordered way are memebers of convex hull.
 * 3. 1.Take closes xMax and yMax points to complete a BB
 *    2. Get all points in bounding box BB with slope larger than diag of BB
 *    3. Transform points to X axis and find points farthest from X ie max Y.
 *    4. Transform highest points in 2 to back to diagonal.
 *    5. Add the points to Convex hull
 *
 *
 *
 *
 */

case class SetGraphNodeNbr(val setupGraphNode: SetGraphNode, val pt: (Int, Int)) {
  var quadTree: QuadTreeImpl = _
}
class SetGraphNode (val pts: List[(Int, Int)]) {
  var nbrs = ListBuffer[SetGraphNodeNbr]()
  var resolved = false

  def addNbr(x: SetGraphNodeNbr) = {
    nbrs += x
  }

  def deleteNbr(y: SetGraphNodeNbr) = {
    nbrs.remove(nbrs.zipWithIndex.find(x => x._1 == y).head._2)
  }
}
class SetGraph() {//its a node view graph with nodes carrying their connections to other nodes
  val nodes = ListBuffer[SetGraphNode]()

  def add_node(node: SetGraphNode): Unit = {
    nodes += node
  }

}

class ConvexHull(pts: List[(Int, Int)]) {
  val mainQuadTree = QuadTreeImpl()
  var maxX: Int = pts.head._1
  var minX: Int = pts.head._1
  var maxY: Int = pts.head._2
  var minY: Int = pts.head._2
  for (pt <- pts) {
    mainQuadTree.insertPoint(pt)
    if (maxX < pt._1) maxX = pt._1
    if (maxY < pt._2) maxY = pt._1
    if (minX < pt._1) minX = pt._1
    if (minY < pt._1) minY = pt._1
  }

  val g = new SetGraph()
  setInitialConvexSets()


  def findMaxPoints(): ((List[(Int,Int)], List[(Int,Int)], List[(Int,Int)], List[(Int,Int)])) = {
    (pts.filter(x => x._1 == maxX), pts.filter(x => x._1 == maxY),
      pts.filter(x => x._1 == minX),pts.filter(x => x._1 == minY))
  }

  def setInitialConvexSets(): Unit = {
    val maxPts = findMaxPoints()
    val x_M = SetGraphNode(maxPts._1)
    val y_M = SetGraphNode(maxPts._2)
    val x_m = SetGraphNode(maxPts._3)
    val y_m = SetGraphNode(maxPts._4)

    g.add_node(x_m)
    g.add_node(x_M)
    g.add_node(y_m)
    g.add_node(y_M)
    val x_M_y_M = SetGraphNodeNbr(y_M, findClosestPoint(x_M.pts, y_M.pts)._1)
    val x_M_y_m = SetGraphNodeNbr(y_m, findClosestPoint(x_M.pts, y_m.pts)._1)
    x_M.addNbr(x_M_y_M)
    x_M.addNbr(x_M_y_m)


    val y_M_x_M = SetGraphNodeNbr(x_M, findClosestPoint(y_M.pts, x_M.pts)._1)
    val y_M_x_m = SetGraphNodeNbr(x_m, findClosestPoint(y_M.pts, x_m.pts)._1)
    y_M.addNbr(y_M_x_M)
    y_M.addNbr(y_M_x_m)

    val x_m_y_M = SetGraphNodeNbr(y_M, findClosestPoint(x_m.pts, y_M.pts)._1)
    val x_m_y_m = SetGraphNodeNbr(y_m, findClosestPoint(x_m.pts, y_m.pts)._1)
    x_m.addNbr(x_m_y_M)
    x_m.addNbr(x_m_y_m)

    val y_m_x_M = SetGraphNodeNbr(x_M, findClosestPoint(y_m.pts, x_M.pts)._1)
    val y_m_x_m = SetGraphNodeNbr(x_m, findClosestPoint(y_m.pts, x_m.pts)._1)
    y_m.addNbr(y_m_x_m)
    y_m.addNbr(y_m_x_M)



    x_M_y_M.quadTree = new QuadTreeImpl(mainQuadTree.getPointsInBox(x_M_y_M.pt, y_M_x_M.pt))
    y_M_x_M.quadTree = x_M_y_M.quadTree


    x_M_y_m.quadTree = new QuadTreeImpl(mainQuadTree.getPointsInBox(x_M_y_m.pt, y_m_x_M.pt))
    y_m_x_M.quadTree = x_M_y_m.quadTree


    x_m_y_M.quadTree = new QuadTreeImpl(mainQuadTree.getPointsInBox(x_m_y_M.pt, y_M_x_m.pt))
    y_M_x_m.quadTree = x_m_y_M.quadTree


    x_m_y_m.quadTree = new QuadTreeImpl(mainQuadTree.getPointsInBox(x_m_y_m.pt, y_m_x_m.pt))
    y_m_x_m.quadTree = x_m_y_m.quadTree


  }

  def findClosestPoint(pts: List[(Int, Int)], pts1: List[(Int, Int)]): ((Int, Int), (Int, Int)) = {
    val ptData = mutable.HashMap[(Int, Int), (Int, (Int, Int))]()
    for (pt <- pts) {
      var minPt = (0, 0)
      var minPtDt = 1000000
      for (pt1 <- pts1) {
        if (minPtDt > distance(pt, pt1)) {
          minPtDt = distance(pt, pt1)
          minPt = pt1
        }
      }
      ptData(pt) = (minPtDt, minPt)
    }
    val minEntry = ptData.minBy(x => x._1)
    (minEntry._1, minEntry._2._2)
  }


  def distance(x: (Int, Int), y: (Int, Int)): Int = {
    (x._1-y._1)*(x._1-y._1) + (x._2-y._2)*(x._2-y._2)
  }

  def orderPointsAxially(p1:(Int, Int), p2: (Int, Int)): ((Int, Int), (Int, Int)) = {
    //one with lower x is first point
    if (p1._1 <= p2._1) {
      ((p1, p2))
    } else{
      ((p2, p1))
    }
  }

  def findConvexHull() = {
    //g is set
    var cond = true
    while (cond) {
      val edgesVisited = mutable.HashSet[(SetGraphNode, SetGraphNode)]()
      val curNodes = g.nodes
      var loopWorked = 0
      for (node <- curNodes) {
        for (nbr <- node.nbrs) {
          if (!edgesVisited.contains((node, nbr.setupGraphNode))) {
            edgesVisited += ((node, nbr.setupGraphNode))
            edgesVisited += ((nbr.setupGraphNode, node))
            val n1 = node
            val n2 = nbr.setupGraphNode
            val nbr_r = n2.nbrs.filter(nbr => nbr.setupGraphNode == node).head
            val transformedPts = transformPtsToAxis(nbr.pt , nbr_r.pt , nbr.quadTree.getAllPoints())
            val maxPtsInTransformedSpace = findMaxYAxisPoints(transformedPts._1)
            val maxPts = maxPtsInTransformedSpace.map (x => transformedPts._2(x))
            val maxPtsFiltered = maxPtsFilteredAboveAxis(nbr.pt, nbr_r.pt, maxPts)
            if (maxPtsFiltered.nonEmpty) {
              //delete old nbrs and create new nbrs
              n1.deleteNbr(nbr)
              n2.deleteNbr(nbr_r)
              val newNode = new SetGraphNode(maxPtsFiltered.toList)
              val closestPts_n1_new = findClosestPoint(n1.pts, newNode.pts)
              val n1_nbr_new = SetGraphNodeNbr(newNode, closestPts_n1_new._1)
              n1.addNbr(n1_nbr_new)
              val newNode_nbr_n1 = SetGraphNodeNbr(n1, closestPts_n1_new._2)
              newNode.addNbr(newNode_nbr_n1)
              //set quadtree of newNode
              newNode_nbr_n1.quadTree = new QuadTreeImpl(mainQuadTree.getPointsInBox(orderPointsAxially(closestPts_n1_new._1, closestPts_n1_new._2)))
              n1_nbr_new.quadTree = newNode_nbr_n1.quadTree


              val closestPts_n2_new = findClosestPoint(n2.pts, newNode.pts)
              val n2_nbr_new = SetGraphNodeNbr(newNode, findClosestPoint(n2.pts, newNode.pts)._1)
              n2.addNbr(n2_nbr_new)
              val newNode_nbr_n2 = SetGraphNodeNbr(n2, findClosestPoint(n2.pts, newNode.pts)._2)
              newNode.addNbr(newNode_nbr_n2)
              newNode_nbr_n2.quadTree = new QuadTreeImpl(mainQuadTree.getPointsInBox(orderPointsAxially(closestPts_n2_new._1, closestPts_n2_new._2)))
              n2_nbr_new.quadTree = newNode_nbr_n1.quadTree
              g.add_node(newNode)
              loopWorked += 1
            }
          } else {
            //already visited
          }

        }

      }
      if (loopWorked == 0) {
        cond = false
      }
    }
  }

  def transformPtsToAxis(p1: (Int, Int), p2: (Int, Int), pts: ListBuffer[(Int, Int)]): (ListBuffer[(Double, Double)],
    mutable.Map[(Double,Double), (Int, Int)]) = {
    val res = mutable.HashMap[(Double,Double), (Int, Int)]()
    val tan = (p2._2-p1._2, p2._1-p1._1)
    val hyp= math.sqrt(tan._1*tan._1 + tan._2*tan._2)
    val cos = (tan._1/hyp)
    val sin = (tan._2/hyp)
    val tps = pts.map { p =>
      val tp = (p._1 * cos  - p._2* sin, p._2* cos + p._1*sin)
      res(tp) = p
      tp
    }
    (tps, res)
  }

  def findMaxYAxisPoints(pts: ListBuffer[(Double, Double)]): ListBuffer[(Double, Double)] = {
    val maxYPtOrdinal = pts.maxBy(p => p._2)._2
    pts.filter(p => p._2 == maxYPtOrdinal)
  }

  def maxPtsFilteredAboveAxis(p1: (Int, Int), p2: (Int, Int), pts: ListBuffer[(Int, Int)]):  ListBuffer[(Int, Int)]  =
    import math._
    val yDiff = p2._2 - p1._2
    val xDiff = p2._1 - p1._1
    (xDiff > 0, yDiff >0) match {
      case (false, false) =>
        pts.filter{ pt =>
          val ydiff_pt = abs(pt._2 - p1._2)
          val xdiff_pt = abs(pt._1 - p1._1)
          if (ydiff_pt* -xDiff < -yDiff* xdiff_pt) {
            true
          } else {
            false
          }

        }
      case (true, false) =>
        pts.filter{ pt =>
          val ydiff_pt = abs(pt._2 - p1._2)
          val xdiff_pt = abs(pt._1 - p1._1)
          if (ydiff_pt* xDiff < -yDiff* xdiff_pt) {
            true
          } else {
            false
          }

        }
      case (false, true) =>
        pts.filter{ pt =>
          val ydiff_pt = abs(pt._2 - p1._2)
          val xdiff_pt = abs(pt._1 - p1._1)
          if (ydiff_pt* -xDiff > yDiff* xdiff_pt) {
            true
          } else {
            false
          }
        }
      case _ =>
        pts.filter{ pt =>
          val ydiff_pt = pt._2 - p1._2
          val xdiff_pt = pt._1 - p1._1
          if (ydiff_pt* xDiff > yDiff* xdiff_pt) {
            true
          } else {
            false
          }
        }

    }
}


