package org.nsinha
package hashfns

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import com.google.common.hash.{Hasher, Hashing}
class ValueOpenAddressHashNode {
  var l: (Int, String) = null
}
class OpenAddressHashTable {
  val INIT_SIZE = 2
  var size = 0
  var capacity: Int = INIT_SIZE
  var valueArray = new Array[ValueOpenAddressHashNode]( Integer(1) << capacity)
  var mask = ((1 << capacity) - 1)
  var collisons = 0
  val LOAD = 0.7
  val hasher = Hashing.sha256()

  def insert(k: Int, v: String) = {
    //println(s"entering a new kv ${k}:${v}")
    if (!insertInternal(k,v, valueArray, mask)) {
      insertInternal(k,v, valueArray, mask)
    }
  }


  def get(key: Int): List[String] = {
    val idx = math.abs(((hasher.hashInt(key).asLong()) & mask).toInt)
    val res = ListBuffer[String]()
    if (valueArray(idx) == null) {
      res.toList
    } else {
      //start searching for first empty slot after idx
      for (k <- Range(idx, valueArray.length)) {
        if (valueArray(k) != null) {
          if ((math.abs(((hasher.hashInt(valueArray(k).l._1).asLong()) & mask).toInt)) == k) { //hash of key same as k so it is end
            if (k == key) {
              res += valueArray(k).l._2
              return res.toList
            } else {
              res.toList
            }
          } else {
            if (valueArray(k).l._1 == k) {
              res += valueArray(k).l._2
            }
          }
        } else {
          return res.toList
        }
      }
      res.toList
    }
  }

  def insertInternal(k: Int, v: String, ar: Array[ValueOpenAddressHashNode], mask: Int, isGrow: Boolean = true): Boolean = {
    val idx = math.abs(((hasher.hashInt(k).asLong()) & mask).toInt)
    if (ar(idx) == null) {
      //println(s"hashIdx= ${idx} is vacant")
      ar(idx) = new ValueOpenAddressHashNode
      ar(idx).l = (k, v)
      return true
    } else {
      //println(s"hashIdx= ${idx} is nonvacant")
      //start searching for first empty slot after idx
      for (kk <- Range(idx + 1, 2 * ar.length - 1)) {
        val k = if (kk >= ar.length) kk - ar.length else kk
        if (ar(k) == null) {
          //println(s"hashIdx= ${k} is vacant")
          ar(k) = new ValueOpenAddressHashNode
          ar(k).l = (k, v)
          return true
        }
      }
    }
    //here means could not find empty slot
    grow(1)
    false
  }


  def grow(pow: Int): Boolean = {
    ////println(s"Grow enter: ${capacity + pow}")
    collisons = 0
    val capacity1: Int = capacity + pow
    val mask1 = (1 << capacity1) - 1
    var valueArray1 = new Array[ValueOpenAddressHashNode](1 << capacity1)
    for (i <- Range(0, 1 << capacity)) {
      if (valueArray(i) != null) {
        //println(s"Rehashing ${valueArray(i).l._1}: ${valueArray(i).l._2}")
        val t = valueArray(i).l
        val r = insertInternal(t._1, t._2, valueArray1, mask1, false)
        if (r == false) {
          //println("Grow aborted")
          return false
        }
      }
    }
    capacity = capacity1
    valueArray = valueArray1
    mask = mask1
    //println("Grow done")
    true
  }
}
