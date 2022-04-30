package org.nsinha
package leetcode.hard

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class FancySequence {

  private val data = ListBuffer[Int]()
  private var nextIdx = 0

  private val map = mutable.TreeMap[Int, (String, Int)]()


  def  append(x: Int) = {
    data += x
    nextIdx += 1
  }

  def len() = {
    data.length
  }

  def addAll(x: Int) = {
    map += (nextIdx -> ("a", x))
  }

  def multAll(x: Int) = {
    map += (nextIdx -> ("m", x))
  }

  def getIndx(idx: Int): Int = {
    var origData = data(idx)
    map.filterKeys( _ > idx).foreach {
      case (_,v) =>
        if (v._1 == "a") {
          origData += v._2
        } else {
          origData *= v._2
        }
    }
    origData
  }

}


object FancySequence {

  def main(args: Array[String]): Unit =
    val fancySeq = new FancySequence()
    fancySeq.append(1)
    fancySeq.addAll(2)
    fancySeq.append(3)
    fancySeq.multAll(5)
    fancySeq.append(7)
    for (i <- Range(0, fancySeq.len())) {
      println(fancySeq.getIndx(i))
    }

}
