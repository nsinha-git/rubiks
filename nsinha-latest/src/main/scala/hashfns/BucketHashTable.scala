package org.nsinha
package hashfns

import com.google.common.hash.Hashing

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.hashing.MurmurHash3.*

class ValueBucketHashNode {
  var l  = mutable.ArrayDeque[(Int, String)]()
}
class BucketHashTable {
  val INIT_SIZE = 2
  var size = 0
  var capacity: Int = INIT_SIZE
  var mask = (1 << capacity) - 1
  var valueArray = new Array[ValueBucketHashNode](1 << capacity)
  var collisons = 0
  val LOAD = 0.7
  val hasher = Hashing.sha256()


  def insert(k: Int, v: String) = {
    insertInternal(k,v, valueArray, mask, true)
  }

  def get(k: Int): List[String] = {
    val idx = math.abs(((hasher.hashInt(k).asLong()) & mask).toInt)
    val res = ListBuffer[String]()
    if (valueArray(idx) != null) {
      for (t <- valueArray(idx).l) {
        if (t._1 == k) {
          res += t._2
        }
      }
    }
    return res.toList

  }

  def insertInternal(k: Int, v: String, ar: Array[ValueBucketHashNode], mask: Int, isGrow: Boolean = true) : Unit = {
    val idx = math.abs(((hasher.hashInt(k).asLong()) & mask).toInt)
    if (ar(idx) == null) {
      ar(idx) = new ValueBucketHashNode
    }
    ar(idx).l += ((k,v))
    if (ar(idx).l.size > 1) {
      collisons += 1
      if (collisons > (mask + 1) * LOAD) {
        if (isGrow) {
          grow()
        } else {
          assert(false)
        }
      }
    }
  }

  def delete(k: Int) = {
    val idx = math.abs(((hasher.hashInt(k).asLong()) & mask).toInt)
    if (valueArray(idx) != null) {
      var idxOfKey = -1
      for (k <- Range(0, valueArray(idx).l.size)) {
        if (valueArray(idx).l(k)._1 == k) {
          idxOfKey = k
        }
      }
      if (idxOfKey > 0) valueArray(idx).l.remove(idxOfKey)
    }
  }

  def grow(): Unit = {
    //println(s"Grow enter ${capacity + 1}")
    collisons = 0
    val capacity1  = capacity + 1
    val mask1 = (1 << capacity1) - 1
    var valueArray1 = new Array[ValueBucketHashNode](mask1 + 1)
    for (i <- Range(0, mask + 1)) {
      if (valueArray(i) != null) {
        for (t <- valueArray(i).l) {
          insertInternal(t._1, t._2, valueArray1, mask1, false)
        }
      }
    }
    capacity = capacity1
    mask = mask1
    valueArray = valueArray1
    //println(s"Grow done")
  }

}
