package leetcode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A password is considered strong if the below conditions are all met:
 *
 * It has at least 6 characters and at most 20 characters.
 * It contains at least one lowercase letter, at least one uppercase letter, and at least one digit.
 * It does not contain three repeating characters in a row (i.e., "...aaa..." is weak, but "...aa...a..." is strong, assuming other conditions are met).
 * Given a string password, return the minimum number of steps required to make password strong. if password is already strong, return 0.
 *
 * In one step, you can:
 *
 * Insert one character to password, I
 * Delete one character from password, D or
 * Replace one character of password with another character. R
 *
 *
 * Example 1:
 *
 * Input: password = "a"
 * Output: 5
 * Example 2:
 *
 * Input: password = "aA1"
 * Output: 3
 * Example 3:
 *
 * Input: password = "1337C0d3"
 * Output: 0
 *
 *
 * Constraints:
 *
 * 1 <= password.length <= 50
 * password consists of letters, digits, dot '.' or exclamation mark '!'.
 */

/**
 * 730
1. I use only if less than 5
2. D is implicit when N> 20
3.Use R to correct violations
LC,UC,NC
1. case |w|<6.as per lc,uc,nc. scan w for RR viloation and at point pos to fix it. every third. for each RR v mark one of LC,UC,NC resolved.
 For remaining c violations, use I.Use I if length still less than 6.
2. (6,20): RR violations markup.Resolve each RR violation with a C violation(max 3).For rmaining RR vilation use R.
3.>20. For every element count length 20. calculate c violations each time or dynamically. Cal num RR violations in efffective way for segment.
max(RR, c) violations if using R. Minimize among all segments.
*/
class PasswordChecker {

 def solve(w: String): Int = {
  if (w.length < 6) {
   cRRViolations(w,0, w.length) + 6 - w.length
  } else if (w.length <= 20) {
   cRRViolations(w,0, w.length)
  } else {
   var min = 100
    for (i <- Range(0, w.length-20)) {
     min = Math.min(cRRViolations(w, i, i + 20), min)
    }
   min
  }
 }

 def cRRViolations(str: String, s: Int, e: Int): Int = {
  var lc = 1
  var uc = 1
  var nc = 1
  var rr = 0
  val rrSet = mutable.HashSet[Int]()
  for (i <- Range(s, e)) {
   val c = str(i)
   if (c.isDigit) nc = 0 else {
    if (c.isLower) lc = 0 else uc = 0
   }
   if (i>1) {
    if (str(i-1) == str(i-2) && c == str(i-1) && !rrSet.contains(i-1) && !rrSet.contains(i-2)) {
     rr += 1
     rrSet += i
    }
   }
  }

  val c = lc +uc + nc
  if (rr > c) rr else c - rr
 }


}
