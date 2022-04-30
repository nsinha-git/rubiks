package org.nsinha
package google.codejam.twentytwo.qaround

object ProblemThree {



  /**
   * arrange the ar in ascending order.
   * start with index 0 len 0 and go up  increasing index and len by 1.checking if entry
   * supports the len, if not drop it and go to next one w/o increasing indx or len. Finish
   */
  def findLongestDiceSequence(ar:Array[Int]): Int = {
    val sorted = ar.sorted
    var len = 0
    for (i<- Range(0, sorted.length)) {
      if (sorted(i) > len ) {
        len +=1
      } else {
      }
    }
    len
  }


  def main(args: Array[String]): Unit = {
    println(findLongestDiceSequence(Array(6, 10, 12, 8)))
    println(findLongestDiceSequence(Array(5,4,5,4,4,4)))
    println(findLongestDiceSequence(Array(10,10,7,6,7,4,4,5,7,4)))
    println(findLongestDiceSequence(Array(10)))
  }
}
