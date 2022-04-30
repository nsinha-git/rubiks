package leetcode

/**
 * Valid Number
Hard

158

319

Add to List

Share
A valid number can be split up into these components (in order):

A decimal number or an integer.
(Optional) An 'e' or 'E', followed by an integer.
A decimal number can be split up into these components (in order):

(Optional) A sign character (either '+' or '-').
One of the following formats:
One or more digits, followed by a dot '.'.
One or more digits, followed by a dot '.', followed by one or more digits.
A dot '.', followed by one or more digits.
An integer can be split up into these components (in order):

(Optional) A sign character (either '+' or '-').
One or more digits.
For example, all the following are valid numbers: ["2", "0089", "-0.1", "+3.14", "4.", "-.9", "2e10", "-90E3", "3e+7", "+6e-1", "53.5e93", "-123.456e789"], while the following are not valid numbers: ["abc", "1a", "1e", "e3", "99e2.5", "--6", "-+3", "95a54e53"].

Given a string s, return true if s is a valid number.
 */

/**
 * N = D + E
 * D = N*|N*.|N*.N*
 * N=[0-9]
 * E = eN*
 */
class ValidNumber {

  def isValid(w: String): Boolean = {
    val deOpt: Option[(String, String)] = breakIntoDE(w)
    deOpt match {
      case Some(de) =>
        val decimal = de._1
        val exp = de._2
        isInt(exp) && isDecimal(decimal)
      case _ => false
    }

  }

  def isDecimal(str: String): Boolean = {
    val deOpt: Option[(String, String)]  = breakIntoDecParts(str)
    deOpt match {
      case Some(de) =>
        val dec1 = de._1
        val dec2 = de._2
        isInt(dec1) && isDecimal(dec2)
      case _ => false
    }

  }

  def isInt(str: String): Boolean =  {
    str.forall(x => x.isDigit)
  }

  def breakIntoDecParts(str: String):  Option[(String, String)] = {
    var idxE = -1

    for ((c,i) <- str.zipWithIndex) {
      if (c == '.' && idxE == -1) {
        idxE = i
      }
    }
    if (idxE != -1) {
      Some(str.substring(0,idxE), str.substring(idxE+1))
    } else {
      Some(str, "")
    }

  }

  def breakIntoDE(str: String): Option[(String, String)] = {
    var idxE = -1
    for ((c,i) <- str.zipWithIndex) {
      if (c == 'e' && idxE == -1) {
        idxE = i
      }
    }
    if (idxE != -1) {
      Some(str.substring(0,idxE), str.substring(idxE+1))
    } else {
      Some(str, "")
    }

  }
}
