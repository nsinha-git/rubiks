package org.nsinha
package hashfns

import java.time.InstantSource.system
import scala.util.{Random, hashing}
import scala.language.postfixOps

object SpeedTest {


  def genKV(cnt : Int): List[(Int, String)] = {
    Range(0, cnt) map { i =>
      (Random.nextInt(100000) , i.toString)
    } toList
  }

  def measureTimings(): Unit = {
    for (i <- Range(0, 10)) {
      val ht1 = new BucketHashTable
      val ht2 = new OpenAddressHashTable
      val listOfKv = genKV(100000)

      val t1 = System.currentTimeMillis()
      for (kv <- listOfKv) {
        ht1.insert(kv._1, kv._2)
      }
      val t2 = System.currentTimeMillis()
      print(t2 - t1 + " ,")

      for (kv <- listOfKv) {
        ht2.insert(kv._1, kv._2)
      }

      val t3 = System.currentTimeMillis()

      print(t3 - t2+ " ,")

      for (kv <- listOfKv) {
        ht1.get(kv._1)
      }
      val t4 = System.currentTimeMillis()
      print(t4 - t3+ " ,")
      for (kv <- listOfKv) {
        ht2.get(kv._1)
      }
      val t5 = System.currentTimeMillis()
      println(t5 - t4)
    }
  }


  def main(args: Array[String]): Unit = {
    measureTimings()
  }



}
