package year2020.main

import jdk.nashorn.internal.ir.Labels

import scala.collection.mutable

/**
 * The Code Jam team's first cryptocurrency, jamcoins, never caught on. This year, we are trying again with hexacoins, which are named for their use of base 16. To "mine" a D-digit hexacoin, one has to work with integers using exactly D base 16 digits, including leading zeroes if needed. Each value represents an integer between 0 and 16D - 1, inclusive. Base 16 digits are represented by the numbers 0 through 9 and the uppercase letters A through F, as usual. For example, F2B, 0C8 and 000 are valid values when D=3, corresponding to the base 10 values 3883, 200 and 0. On the other hand, 1234, DF, C0DE and JAM are not valid values when D=3.
 * When performing addition of D-digit base 16 values, any overflow digits are dropped. That is, the addition is performed modulo 16D. For example, F2B + 0C8 = FF3 (4083 in base 10) and F2B + F2B = E56 (3670 in base 10, because the sum's result is 7766, and taking modulo 163 yields 3670).
 * To "mine" a D-digit hexacoin, a computer must perform the following steps:
 * Choose a list L of N D-digit base 16 values L1, L2, ..., LN.
 * Choose a target range of D-digit base 16 values: the numbers from S to E, inclusive.
 * Choose a permutation P of the base 16 digits 0 through F, uniformly at random from among all 16! such permutations.
 * Apply P to all digits of all numbers in the list, creating a new list L' consisting of N D-digit base 16 values. Formally, the j-th digit of the i-th element of L' is the result of applying P to the j-th digit of the i-th element of L.
 * Choose a pair of elements from L' without replacement, uniformly at random from among all such possible choices, and independently of the choice of permutation.
 * Calculate the sum (dropping overflow digits) of the two chosen elements.
 * If the sum calculated in the last step is between S and E, inclusive, then a hexacoin has been found! For example, suppose that:
 * L = [134, 000, FFB, 000, AA9].
 * S = 85C and E = EDF.
 * The computer happens to choose P = (0 → 4, 1 → A, 2 → 2, 3 → 8, 4 → 9, 5 → B, 6 → C, 7 → 7, 8 → F, 9 → 1, A → 0, B → 3, C → 5, D → 6, E → E, F → D).
 * Then, when P is applied to L, the resulting L' is [A89, 444, DD3, 444, 001]. Notice that P is not applied to S and E.
 * There are (5 × 4) / 2 = 10 pairs of values to choose, and each pair has a probability of 1/10 of being chosen. The only sums that fall within the range are A89 + DD3 = 85C, 444 + 444 = 888, A89 + 001 = A8A, DD3 + 001 = DD4, and A89 + 444 = ECD (twice).
 * The first two steps are already computed and you know the list L and the range [S, E] that were chosen. What is the probability that a hexacoin is found after the rest of the process is performed?
 * Input
 * The first line of the input gives the number of test cases, T. T test cases follow. Each test case consists of three lines. The first line contains two integers N and D: the size of the given list and the number of digits to work with, respectively. The second line contains two D-digit base 16 numbers S and E: the inclusive lower and upper bounds of the target range, respectively. Then there is one more line containing N D-digit base 16 numbers L1, L2, ..., LN, representing the values in the list.
 * Output
 * For each test case, output one line containing Case #x: y z, where x is the test case number (starting from 1) and y and z are non-negative integers, such that the fraction y/z represents the probability of finding a hexacoin, under the conditions described above. All of x, y, and z must be in base 10. If there are multiple acceptable values for y and z, choose the ones such that z is minimized.
 * Limits
 * Time limit: 90 seconds per test set.
 * Memory limit: 1GB.
 * 2 ≤ N ≤ 450.
 * S contains exactly D characters.
 * Each character of S is a base 16 digit.
 * E contains exactly D characters.
 * Each character of E is a base 16 digit.
 * S ≤ E.
 * Li contains exactly D characters, for all i.
 * Each character of Li is a base 16 digit, for all i.
 * Test Set 1 (Visible Verdict)
 * 1 ≤ T ≤ 100.
 * 2 ≤ D ≤ 3.
 * Test Set 2 (Hidden Verdict)
 * 1 ≤ T ≤ 100.
 * 2 ≤ D ≤ 4.
 * Test Set 3 (Hidden Verdict)
 * 1 ≤ T ≤ 10.
 * 2 ≤ D ≤ 5.
 * Sample
 *
 * Input
 *
 * Output
 *
 * 4
 * 2 2
 * 10 10
 * 00 FF
 * 2 2
 * 10 11
 * 00 FF
 * 4 3
 * FFF FFF
 * 230 A10 010 F70
 * 4 3
 * AFF FFF
 * 230 A10 010 F70
 *
 *
 * Case #1: 7 120
 * Case #2: 1 15
 * Case #3: 0 1
 * Case #4: 2731 8736
 *
 */
/** N,D
 * S,E
 * L
 * 1. Given S,E and list L of length N, a digit D  and space of P
 * 2. For t(target)=S,E;(Bf)
 *    1. for each e in P generate list Le. generate nc2 pairs from Le and test and count + , -
 *    2. p= sig(+)/{all}
 *    3. There are 16! elements in P. so 16!Nc2 i.e 16!450C2 = 40k*450*449=~e10
 *  3. 16! is way too big.
             1.  as we add two numbers together, we can just relabel it in terms of k0,k1,....e.g(k0*2, k1+k2, K2+k1, ...)
             2.  any other pair which has same distribution is same as the other label
             3. start msb.go down.shift the bounds. at start original bound, no assignment, to do.
             4. if 2*kt then choose kt from boundor one lower. if kt + kt-1 chhose kt,kt-1 together for bound or one under.
             5. for next there is a new bound, a assignment, and to do.keep drilling. using any dynamic efficiency.
             6. to take care of lower bounds with carry happening in middle we will extend bounds by 1 more digit stuck at zero in beginning but used to
                keep any carry.
 */
class TestThree(S: String, E: String, L: List[String], D: Int) {
  val t1 = mutable.HashMap[String, (Int, Int)]()
  val symbols: Set[String] = Set("a", "b", "c", "d", "e", "f", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")

  def add(a: String, b: String): Unit = {
    val t1 = hextoInt(a)
    val t2 = hextoInt(b)
    val res = t1 + t2
    val c = if (res >= 16) {1} else {0}
    (c, intToHex(res%16))
  }

  def combinations(i: Int): Int = {
    var res = 1
    for (j <- Range(0,i)) {
      res = res * (16-j)
    }
    res
  }

  def process(): Double = {
    var total = 0
    var allTotal=0
    var positives = 0
    for ( i <- Range(0,  L.size)) {
      for (j <- Range(i, L.size)) {
        //add list elems at i and j
        val (relabels, totLabels) = relabel(L(i), L(j))
        val (p, n): (Int, Int) = calc(relabels, '0' + S,  E, Map())
        positives += p
        total += (p + n)
        allTotal += combinations(totLabels)
      }
    }
    println(s"$positives $allTotal")
    println(reduceToLeastFactors(positives, allTotal))
    println(s"hashsz=${t1.size}")
    positives/total

  }

  /**
   *
   * labels contains pairs of (ko,k1), ..(k2d-2, k2d-1) based on makeup and correspondance of a,b
   * @param a
   * @param b
   * @return
   */
  def relabel(a: String, b: String): (List[(Int, Int)], Int) = {
    val seenSet = mutable.HashMap[String, Int]()
    var next = 0
    //labels contains pairs of (ko,k1), ..(k2d-2, k2d-1)
    val labels = for (i <- Range(0, D)) yield {
      //look at ith digit from left of a and b
      val ai = hextoInt(a(i).toString)
      val bi = hextoInt(b(i).toString)
      val p = intToHex(math.min(ai, bi)) //min of two
      val q = intToHex(math.max(ai, bi)) //max of two
      val res = mutable.ListBuffer[Int]()
      if (!seenSet.contains(p)) {
        //assign a label to min
        seenSet(p) = next
        res += next //res now has this label
        next += 1
      } else {
        res += seenSet(p)
      }
      if (!seenSet.contains(q)) {
        //assign a label to max
        seenSet(q) = next
        res += next //res now has this label
        next += 1
      } else {
        res += seenSet(q)
      }

      (res(0), res(1))
    }
    (labels toList, next)
  }

  def hextoInt(x: String): Int = {
    Integer.parseInt(x, 16)
  }

  def intToHex(x: Int): String = {
    Integer.toHexString(x)
  }

  /**
   *
   * @param labels
   * @param lower
   * @param upper
   * @param assigned:
   * @return (positives, total)
   */
  def calc(labels: List[(Int, Int)], lower: String, upper: String, assigned: Map[Int, Int]): (Int, Int) = {
    val hash = hashIt(labels, lower, upper, assigned)
    if (t1.contains(hash)) {
      t1(hash)
    } else {
      var res1 = 0
      var res2 = 0
      val baseCase = labels.size == 1
      //take the first unprocessed el
      val (l1, l2) = labels(0)
      if (assigned.getOrElse(l1, -1) == -1) { //l1Val not assigned
        if (lower(0) == '0') {
          for (i <- Range(0, hextoInt(lower(1).toString) + 1)) {
            val assigned1 = assigned ++ List(l1 -> i)
            val jList = if (assigned1.getOrElse(l2, -1) == -1) {
              List(hextoInt(lower(1).toString) - 1 - i, hextoInt(lower(1).toString) - i) filter (x => x >= 0)
            } else {
              List(assigned1.getOrElse(l2, -1))
            }
            for (j <- jList) {
              val assigned2 = assigned1 ++ List(l2 -> j)
              if (adjUpper(upper, i, j) != "-Z") {
                if (!baseCase) {
                  val (pos, tot) = calc(labels.tail, adjLower(lower, i, j), adjUpper(upper, i, j), assigned2)
                  res1 = res1 + pos
                  res2 = res2 + tot
                } else {
                  val sum = i + j
                  if (sum >= hextoInt(lower(1).toString)) {
                    if (upper == "Z" || sum <= hextoInt(upper)) {
                      res1 += 1
                      res2 += 1
                    } else  {
                      res1 += 0
                      res2 += 1
                    }
                  } else {
                    res1 += 0
                    res2 += 1
                  }
                }
              }
            }
          }
        } else { //carry must happen
          for (i <- Range(0, 16)) {
            val assigned1 = assigned ++ List(l1 -> i)
            val jList = if (assigned1.getOrElse(l2, -1) == -1) {
              Range(0, 16) filter (x => x + i >= 15)
            } else {
              List(assigned1.getOrElse(l2, -1))
            }
            for (j <- jList) {
              val assigned2 = assigned1 ++ List(l2 -> j)
              if (adjUpper(upper, i, j) != "-Z") {
                if (!baseCase) {
                  val (pos, tot) = calc(labels.tail, adjLower(lower, i, j), adjUpper(upper, i, j), assigned2)
                  res1 = res1 + pos
                  res2 = res2 + tot
                } else {
                  val sum = i + j
                  if (sum < 16) {
                    res1 += 0
                    res2 += 1
                  } else if (sum >= hextoInt(lower(1).toString)) {
                    if (upper == "Z" || sum <= hextoInt(upper)) {
                      res1 += 1
                      res2 += 1
                    } else  {
                      res1 += 0
                      res2 += 1
                    }
                  } else {
                    res1 += 0
                    res2 += 1
                  }
                }
              }
            }
          }
        }
      } else { //l1val is assigned
        if (lower(0) == '0') {
          for (i <- List(assigned.getOrElse(l1, -1))) {
            val jList = if (assigned.getOrElse(l2, -1) == -1) {
              List(hextoInt(lower(1).toString) - 1 - i, hextoInt(lower(1).toString) - i) filter (x => x >= 0)
            } else {
              List(assigned.getOrElse(l2, -1))
            }
            for (j <- jList) {
              val assigned2 = assigned ++ List(l2 -> j)
              if (adjUpper(upper, i, j) != "-Z") {
                if (!baseCase) {
                  val (pos, tot) = calc(labels.tail, adjLower(lower, i, j), adjUpper(upper, i, j), assigned2)
                  res1 = res1 + pos
                  res2 = res2 + tot
                } else {
                  val sum = i + j
                  if (sum >= hextoInt(lower(1).toString)) {
                    if (upper == "Z" || sum <= hextoInt(upper)) {
                      res1 += 1
                      res2 += 1
                    } else  {
                      res1 += 0
                      res2 += 1
                    }
                  } else {
                    res1 += 0
                    res2 += 1
                  }
                }
              }
            }
          }
        } else { //carry must happen
          for (i <- List(assigned.getOrElse(l1, -1))) {
            val jList = if (assigned.getOrElse(l2, -1) == -1) {
              Range(0, 16) filter (x => x + i >= 15)
            } else {
              List(assigned.getOrElse(l2, -1))
            }
            for (j <- jList) {
              val assigned2 = assigned ++ List(l2 -> j)
              if (adjUpper(upper, i, j) != "-Z") {
                if (!baseCase) {
                  val (pos, tot) = calc(labels.tail, adjLower(lower, i, j), adjUpper(upper, i, j), assigned2)
                  res1 = res1 + pos
                  res2 = res2 + tot
                } else {
                  val sum = i + j
                  if (sum < 16) {
                    res1 += 0
                    res2 += 1
                  } else if (sum >= hextoInt(lower(1).toString)) {
                    if (upper == "Z" || sum <= hextoInt(upper)) {
                      res1 += 1
                      res2 += 1
                    } else  {
                      res1 += 0
                      res2 += 1
                    }
                  } else {
                    res1 += 0
                    res2 += 1
                  }
                }
              }
            }
          }
        }

      }
      t1(hash) = (res1,res2)
      (res1,res2)
    }


  }

  /**
   *
   * @param lower: has a leading carry 'c'lllll format. c=0 means lower bound is normal. c=1 means '1'0000
   * @param el1
   * @param el2
   * @return
   */
  def adjLower(lower: String, el1: Int, el2: Int): String = {
    assert(lower.size > 1)
    val sum = el1 + el2
    val lowerLimit = hextoInt(lower(1).toString)
    val carry = lower(0) == '1'

    if (carry) {
      //lower.tail exists
      if (sum >= 16) {
        Range(0,lower.tail.size - 1) .map (x => "0").reduceLeftOption((x,y) => x + y) match {
          case Some(t) => "0" + t
          case _ => "0"
        }
      } else {
        Range(0, lower.tail.size - 1).map(x => "0").reduceLeftOption((x, y) => x + y) match {
          case Some(t) => "1" + t
          case _ => "1"
        }
      }
    } else {
        if (sum >= lowerLimit) {
          "0" + lower.tail.tail
        } else {
          Range(0, lower.tail.tail.size).map(x => "0").reduceLeftOption((x, y) => x + y) match {
            case Some(t) => "1" + t
            case _ => "1"
          }
        }
    }
  }

  /**
   * upper needs to be considered till first time its lower, then dont check,
   * no carry to be considered, -Z this seq can not go on,
   * @param upper = "Z" dont check or "aaaaa"
   * @param el1
   * @param el2
   * @return
   */

  def adjUpper(upper: String, el1: Int, el2: Int): String = {
    val sum = el1 + el2
    if (upper == "Z") {
      "Z"
    } else {
      val upperInt = hextoInt(upper(0).toString)
      if (sum > upperInt) {
        "-Z"
      } else if (sum == upperInt) {
        //no carry from lower digits constraint
        upper.tail
      }  else {
        "Z"
      }
    }

  }

  def hashIt(labels: List[(Int, Int)], lower: String, upper: String, assigned: Map[Int, Int]): String = {
    val resBuilder = StringBuilder.newBuilder
    for (label <- labels) {
      resBuilder.append(label._1)
      resBuilder.append(label._2)
    }
    resBuilder.append(lower)
    resBuilder.append(upper)

    for (ass <- assigned) {
      resBuilder.append(1001*ass._1 + ass._2)
    }
    resBuilder.result()
  }

  def reduceToLeastFactors(m: Int,n: Int): (Int, Int) = {
    val f = gcd(m,n)
    if (f ==0) {
      (m,n)
    } else {
      (m / f, n / f)
    }
  }

  def gcd(m: Int, n: Int): Int = {
    if (m <=1) return m
    if ((n/m)*m ==n) {
      m
    } else {
      gcd (n-(n/m)*m, m)
    }

  }

}

object TestThree extends App {

  new TestThree("1", "1", List("0", "F"), 1).process()
  new TestThree("10", "10", List("00", "FF"), 2).process()
  new TestThree("11", "11", List("00", "FF"), 2).process()
  new TestThree("00", "ff", List("00", "FF"), 2).process()

}
