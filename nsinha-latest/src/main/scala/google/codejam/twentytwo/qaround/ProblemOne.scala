package org.nsinha
package google.codejam.twentytwo.qaround

/***
 *
 *
 * A cell has 3 cols and three rows
 * When another cell joins a cell col wise the support col is dropped.
 * when mega cells join each other rowwise the support row is dropped
 * We can think of this a simple join ops. We can dedupe rows and cols which are adjacemt and similar.
 *
 *
 */
class ProblemOne {

  def strTimes(i: Int, str: String) = {
    val res = new StringBuilder()
    for (j <- Range(0, i)) {
      res.append(str)
    }
    res.toString()
  }

  def getRowOfLen(i: Int, mode: Boolean, rowCnt: Int): String = {
    val strStart = "X-X"
    val strStart_isFirst="..X"
    val strStart_1 = "|.|"
    val strStart_1_isSecond = "..|"
    val strStartSuffix = "-X"
    val strStart_1Suffix = ".|"
    if (mode) {
      if(rowCnt == 0) {
        strStart_isFirst  + strTimes((i - 3) / 2, strStartSuffix)
      } else {
        strStart + strTimes((i - 3) / 2, strStartSuffix)
      }
    } else {
      if (rowCnt==1) {
        strStart_1_isSecond + strTimes((i - 3) / 2, strStart_1Suffix)
      } else {
        strStart_1 + strTimes((i - 3) / 2, strStart_1Suffix)
      }
    }
  }

  def generateAscii(r: Int, c: Int) =
    val rowsOfAscii = 2*r + 1 //3,5,7,9
    val colsOfAscii = 2*c + 1
    var mode = true
    var rowCnt = 0
    for (i <- Range(0, rowsOfAscii)) {
      println(getRowOfLen(colsOfAscii, mode, rowCnt))
      mode = !mode
      rowCnt += 1
    }

}


object ProblemOne {
  def main(args: Array[String]): Unit = {
    val problemOne = new ProblemOne()
    problemOne.generateAscii(3,4)
  }
}
