package org.nsinha
package leetcode.hard

import scala.collection.mutable

class MaxPointsOnLine(pts: Array[(Int, Int)]) {
  private val mapdata = mutable.HashMap[(Float, Float), Int]()

  private val mapdataParY = mutable.HashMap[(Float, Float), Int]()

  def getMax(): Int = {
    for (pt <- pts) {
      for (pt1 <- pts if pt1 != pt) {
        val mappped = mapTheLine(pt, pt1)
        if (mappped._1 == null) {
          if (!mapdataParY.contains(mappped._2)) mapdataParY(mappped._2) = 1 else mapdataParY(mappped._2) +=1
        } else {
          if (!mapdata.contains(mappped._2)) mapdata(mappped._2) = 1 else mapdata(mappped._2) +=1
        }
      }
    }
    findMax(mapdata, mapdataParY)
  }

  def findMax(m1: mutable.HashMap[(Float, Float), Int], m2: mutable.HashMap[(Float, Float), Int]): Int = {
    var max = 0
    var maxIdx: Tuple2[Float,Float] = (0,0)
    m1.foreach {
      case (p, f) =>
        if (f > max) {
          max = f
          maxIdx = p
        }
    }

    m2.foreach {
      case (p, f) =>
        if (f > max) {
          max = f
          maxIdx = p
        }
    }
    max
  }

  def mapTheLine(p1: (Int, Int), p2: (Int, Int)): ((Float, Float), (Float, Float)) = {
    val x1 = p1._1
    val y1 = p1._2
    val x2 = p2._1
    val y2 = p2._2
    val slopeNum = y2 - y1
    val slopeDen = x2 - x1
    if (slopeDen == 0) {
      (null, (x1, 0))
    } else {
      val m = slopeNum / slopeDen
      val c = y2 - m * x1
      ((-c / m, m), null)
    }
  }


}
