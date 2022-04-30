package org.nsinha
package leetcode.hard

import scala.collection.mutable.ListBuffer

object NextPerm {
  /***
   * split the seq into I D patterns where I means all numbers are increaseing
   * D means all numbers are decreasing
   * @param ar
   */

  def next(ar:Array[Int]): Array[Int] = {
    val n = ar.length
    if (n==2) {
      return ar.reverse
    }

    val pattern = ListBuffer[(String, List[Int])]()
    var prev_1 = ar.head
    var prev = ar.tail.head
    var curSeqType = if (prev_1<prev) "INC"  else "DEC"
    var emergingPattern = ListBuffer(prev_1, prev)

    for (cur <- ar.tail.tail){
      if (prev < cur) {
        if (curSeqType == "INC") {
          emergingPattern += cur
        } else {
          pattern += ((curSeqType, emergingPattern.toList))
          curSeqType = "INC"
          emergingPattern = ListBuffer(prev, cur)
        }
      } else { //dec seq
        if (curSeqType == "INC") {
          pattern += ((curSeqType, emergingPattern.toList))
          curSeqType = "DEC"
          emergingPattern = ListBuffer(prev, cur)
        } else {
          emergingPattern += cur
        }

      }
      prev = cur
    }
    pattern += ((curSeqType, emergingPattern.toList))

    val flip = if (pattern.last._1 == "INC") {
      0
    } else if (pattern.length > 1 ) {
      1
    } else {
      -1
    }
    if (flip == -1) ar.reverse else reconstruct(pattern map (_._2), flip)
  }

  def reconstruct(seqs_ : ListBuffer[List[Int]], flip: Int): Array[Int] = {
    val seqs = ListBuffer[List[Int]]()
    for (i<- Range(0, seqs_.length)) {
      if (i == 0) {
        seqs += seqs_(i)
      } else {
        //drop the first one
        seqs += seqs_(i).tail
      }

    }
    val flipFromFront = seqs.length - 1 - flip
    val res = ListBuffer[Int]()
    for (i <- Range(0, seqs.length)) {
      if (i == flipFromFront) {
        val data = seqs(i)
        data.zipWithIndex.findLast(x => x._1 < data.last) match {
          case None => data.reverse.foreach(x => res += i)
          case Some(t) => for (i <- Range(0, t._2)) {
            res += data(i)
          }
          res += data.last
          for (i <- Range(t._2+1, data.length-1)) {
              res += data(i)
            }
          res += data(t._2)
        }
      } else
        seqs(i).foreach(x => res += x)
    }
    res.toArray
  }


  def main(args: Array[String]): Unit = {
    println(next(Array(3,2,1,4,5)).toList)
    println(next(Array(3,1,2,4,5)).toList)
  }

}
