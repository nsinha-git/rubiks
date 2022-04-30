package leetcode;
import math._
import scala.collection.immutable.Range

/**
 * Commentary:
 * 1. The issues were with low level edge of array mainpulation based on index 0.
 * 2. Another issue was to move array indexed to lesser or Equal based on pivot.
 * 3. Another issue was to move idx based on equal elements in array
 */

object MedianTwoLists {
  /**
   * Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two sorted arrays.
   *
   * The overall run time complexity should be O(log (m+n)).
   */

  def solve(l1: Array[Int], l2: Array[Int]): Unit = {
    /**
     * val l1 = [1,2,3,4,5]
     * val l1 = [1,2,3,4,5]
     * median M = (n1+n2)/2
     *  1. Use a idx1 and idx2 at n1 , n2 respectively
     *     2. evaluate l1 at idx1 =v1 and l2 at idx2=v2
     *     3. if  v1 > v2 then offset eff_l1 = l1 +l2.
     *     4.3v1>v2. if eff_l1=M1 exit else eff_l1>M then left = l2 with eff_left=eff_l1 and right = l1 and eff_l2=unknown
     *     else ...
     *
     * 5.3v1>v2. evalate l1= l2/2 and so on.
     */

    val m1: Int = math.round((l1.length + l2.length + 1) / 2) //(5 + 5 + 1)/2 = 5
    val m2: Int = math.round((l1.length + l2.length + 2) / 2) //(5 + 5 + 2)/2 = 6
    if (m1 == m2) {
      findNthElem(l1, l2, m1 - 1, 0, l1.length - 1, 0, l2.length - 1)
    } else {
      findNthElem(l1, l2, m1 - 1, 0, l1.length - 1, 0, l2.length - 1)
      findNthElem(l1, l2, m2 - 1, 0, l1.length - 1, 0, l2.length - 1)
    }


  }

  def spanIdZero(i: Int) = {
    i + 1
  }

  def addSpans(a: Int, b: Int) = {
    //2 and 2  0,1,2 and 3,4,5
    //what is idx of 5 in merged array:5
    a + b + 1
  }

  def subtractSpans(a: Int, b: Int) = {
    // let a = 5 and b = 1 then 0,1(b),2,3,4,5(a)
    a - b
  }

  def findNthElemInOneArray(l: Array[Int], i1: Int, i2: Int, n: Int) = {
    println(l(n))
  }

  //all indices are index-0 notation
  //index -0 notation maths. a, span(a) = a + 1
  def findNthElem(l1: Array[Int], l2: Array[Int], n: Int, i1: Int,
                  i2: Int, j1: Int, j2: Int): Unit = {
    /**
     * i1, i2
     * j1, j2
     * n
     * --------
     * if ( len.l1 >=3) || (len.l2 >=3 ) Large length case
     * choose n the element of each. p=l1[i1 + len1/2] and q=l2 [i2 + len2/2]
     * 1.
     *  a. based on p = q:
     *     aa. then index of p is len1/2 + len2/2. q is index(p)-1. if indx(p) < n: the i1' = indx(p), n' = n-(len1/2) -(len2/2), j1'= indx(p) etc
     *     ab. if index(p) -1 = n  or index(p) = n return with p
     *     ac. if index(p) -1 > n: i2' = len1/2 and j2= len2/2
     *     b. p > q:
     *     ba. indx(p) = len1/2 + move array to lowest on l2 st. less than p.  movedLen/2 indx(q) = unknown. if index(p) == n return
     *     bb. indx(p) < n: i1 = len1/2, j1 = next element equal or greater to p.
     *     bc. indx(p) > n: i2= len1/2,
     *     c. q>p symmetric to b.
     */
    if (i2 == -1) {
      findNthElemInOneArray(l2, j1, j2, n)
    } else if (j2 == -1) {
      findNthElemInOneArray(l1, j1, j2, n)
    } else if (spanIdZero(j2 - j1) > 3 | spanIdZero(i2 - i1) > 3) {
      val midl1 = i1 + math.round((i2 - i1) / 2).asInstanceOf[Int] //valid only when i2-i1 >= 2
      val midl2 = j1 + math.round((j2 - j1) / 2).asInstanceOf[Int] //valid only when i2-i1 >= 2
      val p = l1(midl1)
      val q = l2(midl2)
      if (p == q) {
        val idxp = addSpans(midl1, midl2)
        val idxq = idxp - 1
        if (idxp == n | idxq == n) {
          println(p)
        } else if (idxp < n) {
          findNthElem(l1, l2, n, i1 + midl1, i2, j1 + midl2, j2)
        } else {
          findNthElem(l1, l2, n, i1, i1 + midl1, j1, j1 + midl2)
        }
      } else if (p > q) {
        //move l2 to equal or lesser than p
        val movedl2 = moveArrayToEqualOrLesser(l2, midl2, j2, p)
        val idxp = addSpans(midl1, movedl2)
        if (idxp == n) {
          println(p)
        } else if (idxp < n) {
          //move j1 to somewhere where it can be equal or just lesser to p
          val j1New = moveArrayToEqualOrLesser(l2, j1, j2, p)
          findNthElem(l1, l2, n, i1 + midl1, i2, j1New, j2)
        } else { //indxp > n
          findNthElem(l1, l2, n, i1, i1 + midl1, j1, j2)
        }
      } else { //q > p
        //move l1 to equal or lesser than p
        val movedl1 = moveArrayToEqualOrLesser(l1, midl1, i2, q)
        val idxq = addSpans(movedl1, midl2)
        if (idxq == n) {
          println(q)
        } else if (idxq < n) {
          //move i1 to somewhere where it can be equal or just lesser to p
          val i1New = moveArrayToEqualOrLesser(l2, j1, j2, p)
          findNthElem(l1, l2, n, i1New, i2, j1 + midl2, j2)
        } else { //indxq > n
          findNthElem(l1, l2, n, i1, i2, j1, j1 + midl2 - 1)
        }
      }
    } else { //small cases
      val m = mergeArray(l1, l2, i1, i2, j1, j2)
      println(m(n - i1 - j1))

    }
  }

  def mergeArray(l1: Array[Int], l2: Array[Int], i1: Int, i2: Int, j1: Int, j2: Int): Array[Int] = {
    val len = i2 - i1 + j2 - j1 + 2
    val res = new Array[Int](len)
    var idx = 0
    for (k <-  Range(i1, i2 + 1)) {
      res(idx) = l1(k)
      idx += 1
    }
    for (k <-  Range(j1, j2 + 1)) {
      res(idx) = l2(k)
      idx += 1
    }
    res.sorted
  }

  /**
   * this code moves the idx of array to a position just equal or right of idx that can be checkNum
   * left is guranteed to be lesser than checkNum
   *
   * @param array
   * @param left
   * @param right
   * @param checkNum
   * @return
   */
  def moveArrayToEqualOrLesser(array: Array[Int], left: Int, right: Int, checkNum: Int): Int = {
    assert(array(0) < checkNum)
    if (array(right) < checkNum) {
      right
    } else if (right - left > 3) {
      val mid = left + math.round((right - left) / 2).asInstanceOf[Int]
      if (array(mid) < checkNum) {
        moveArrayToEqualOrLesser(array, mid, right, checkNum)
      } else if (array(mid) == checkNum) {
        rightmostEqual(array, checkNum, mid, right)
      } else {
        moveArrayToEqualOrLesser(array, left, mid, checkNum)
      }
    } else {
      for (idx <- Range(left, right + 1)) {
        if (array(idx) > checkNum) {
          return idx - 1
        }
      }
      right
    }
  }
  /*
  left is guaranteed to be equal to check
   */
  def rightmostEqual(array: Array[Int], check: Int, left: Int, right: Int): Int = {
    if (array(right) == check) {
      right
    } else if (array.length > 3) {
      val mid = left + math.round((right - left) / 2).asInstanceOf[Int]
      if (array(mid) == check) {
        rightmostEqual(array, check, mid, right)
      } else {
        rightmostEqual(array, check, left, mid)
      }
    } else {
      for (idx <- Range(right, left - 1, -1)) {
        if (array(idx) == check) {
          return idx
        } else {}
      }
      assert(false, "unreachable code")
      0
    }
  }
}

object Test extends App {
    //MedianTwoLists.solve(Array(1,2,3,4,5,6,7), Array(1,2,3,4,5,6,7))

    //MedianTwoLists.solve(Array(1,2,3,4,5,6,7), Array(8,9,10))
    //MedianTwoLists.solve(Array(1,2,3,4,5,6,7), Array(5,5,5,5))
    MedianTwoLists.solve(Array(1,2), Array(3,4))
}
