package org.nsinha
package leetcode.hard

import scala.collection.mutable

/**
 * 420. Strong Password Checker
 *Hard
 *
 *466
 *
 *1240
 *
 *Add to List
 *
 *Share
*A password is considered strong if the below conditions are all met:
 *
 *It has at least 6 characters and at most 20 characters.
*It contains at least one lowercase letter, at least one uppercase letter, and at least one digit.
*It does not contain three repeating characters in a row (i.e., "...aaa..." is weak, but "...aa...a..." is strong, assuming other conditions are met).
*Given a string password, return the minimum number of steps required to make password strong. if password is already strong, return 0.
 *
 *In one step, you can:
 *
 *Insert one character to password,
*Delete one character from password, or
*Replace one character of password with another character.
 *
 */

/**
 * More/Less length: Strategy ID
 * More than 3: Strategy IORD
 * ID can be positive or negative. IORD can only be positive
 * Fixing missing sets: IONLY
 *
 * cycle:
 * if (ID!=0 and IORD>0 and IONLY >0):
 * if (ID<0) subtract min(-ID, IONLY) IONLY = IONLY - min(-ID, IONLY) if (IONLY < 0) ID +=IONLY and IONLY = 0
 *
 *  if ID > 0:
 *  and ID >= IORD: IORD=0  else:IORD-=ID
 *  done
 *
 *  else if ID <0:
 *  and -ID >=IORD: IORD=0 else IORD+=ID
 *  done
 *
 *
 *
 *
 *
 *
 */
class PasswordChecker {

  def checkCharacterLength(x: String): Int = {
    if (x.length > 20) {
      return x.length - 20
    } else if (x.length < 6) {
      x.length - 6
    } else {
      0
    }
  }

  def checkConsecutive(x: String): Int = {
    var insdel = 0
    for (i <- Range(0, x.length)) {
      val t = x(i)
      if (i < x.length-2) { //aaa,2, i=0
        if (t == x(i+1) && t == x(i+2)) {
          insdel += 1
        }
      }
    }
    insdel
  }

  def checkMembership(x: String): Int = {
    var u = mutable.ListBuffer[Char]()
    var l = mutable.ListBuffer[Char]()
    var d = mutable.ListBuffer[Char]()

    for (c <- x) {
      if (c.isDigit) {
        d += c
      } else if(c.isLower) {
        l += c
      } else {
        u += c
      }
    }
    var res = 0
    if (u.isEmpty) res += 1
    if (l.isEmpty) res += 1
    if (d.isEmpty) res += 1
    res
  }


  def findMinTransformations(x: String): Int = {
    import math._
    var ID  = checkCharacterLength(x)
    var IORD = checkConsecutive(x)
    var IONLY = checkMembership(x)

    var cond = true
    while(cond) {
      if (ID!=0 && IONLY >0) {
        if (ID < 0) {
          IONLY = IONLY - min(-ID, IONLY)
          if (IONLY < 0) {
            ID += IONLY
            IONLY = 0
          }
        } else if (ID > 0) {
          if (IORD > 0) {
            if (ID >= IORD) {
              IORD = 0
            } else {
              IORD -= ID
            }
          } else if (ID < 0) {
            if (-ID >= IORD) {
              ID += IORD
            } else {
              IORD += ID
            }
          }
          cond = false
        } else {
          cond = false
        }

      }
    }
    
    return abs(ID) + IORD + IONLY

  }


}
