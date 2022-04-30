package org.nsinha
package hashfns

import com.google.common.base.Charsets
import com.google.common.hash.Hashing

import scala.io.Source

case class BloomFilter(power: Int, numHashes: Int) {
  val hasher = Hashing.sha1()
  val mask = (1 << 6) - 1
  val maskHash: Long = (1L << power) - 1L

  var data = new Array[Long](Integer(1) << (power-6))


  def set(bitIndex: Long): Unit = {
    var longIndex: Int = (bitIndex >> 6).toInt
    if (longIndex < 0) longIndex = ~longIndex
    val bitIndexInLong = bitIndex & mask
    data(longIndex) = (1 << bitIndexInLong) ^ data(longIndex)
  }

  def get(bitIndex: Long): Boolean = {
    var longIndex: Int = (bitIndex >> 6).toInt
    if (longIndex < 0) longIndex = ~longIndex
    val bitIndexInLong = bitIndex & mask
    val t1: Long = (1 << bitIndexInLong)
    val t2: Long = data(longIndex)
    (t1 & t2) > 0
  }

  def store(w: String) = {
    val hash = math.abs(((hasher.hashString(w, Charsets.UTF_8).asLong()) & maskHash))
    val h2 = hash >> 10
    for (i <- Range(0, numHashes)) {
      var combinedHash = hash + ((i * h2) & maskHash)
      if (combinedHash < 0) combinedHash = ~combinedHash
      val index = combinedHash & maskHash
      set(index)
      if (!get(index)) {
        println(index.toString)
      }
    }
  }


  def check(w: String): Boolean = {
    val hash = math.abs(((hasher.hashString(w, Charsets.UTF_8).asLong()) & maskHash).toInt)
    val h2 = hash >> 10
    var found = true
    for (i <- Range(0, numHashes)) {
      var combinedHash = hash + ((i * h2) & maskHash)
      if (combinedHash < 0) combinedHash = ~combinedHash
      found &= get(combinedHash & maskHash)
    }
    found
  }

}


object BloomFilter {

  def main(args: Array[String]): Unit = {
    val bloomFilter = BloomFilter(30, 3)
    Source.fromFile("/usr/share/dict/words").getLines().foreach(w => {
      bloomFilter.store(w)
    })

    Source.fromFile("/usr/share/dict/words").getLines().foreach(w => {
      if (bloomFilter.check(w)) {
        println("Found")
      } else {
        //println(s"${w} not found")
      }
    })

  }

}
