package org.nsinha
package leetcode.hard

import scala.collection.mutable

object Divison {
  def sum(a: String, b: String): String = {
    val aStack = new mutable.Stack[Int]()
    val bStack = new mutable.Stack[Int]()
    for (i <- a.reverse) {
      aStack += i
    }
    for (i <- b.reverse) {
      bStack += i
    }
    var carry = 0
    val res = new StringBuilder()

    while (aStack.nonEmpty || bStack.nonEmpty) {
      val aDigit = if (aStack.nonEmpty) {
        aStack.pop().toInt
      } else {
        0
      }

      val bDigit = if (bStack.nonEmpty) {
        bStack.pop().toInt
      } else {
        0
      }

      val summandWithCarry = aDigit + bDigit + carry
      val summand = summandWithCarry % 10
      carry = if (summandWithCarry > summand) 1 else 0
      res += summand.toString.charAt(0)
    }
    if (carry == 1) {
      res += '1'
    }

    res.toString()
  }


  def sub(a: String, b: String): String = {
    val aStack = new mutable.Stack[Int]()
    val bStack = new mutable.Stack[Int]()
    for (i <- a.reverse) {
      aStack += i
    }
    for (i <- b.reverse) {
      bStack += i
    }
    var carry = 0
    val res = new StringBuilder()

    while (aStack.nonEmpty || bStack.nonEmpty) {
      val aDigit = if (aStack.nonEmpty) {
        aStack.pop().toInt
      } else {
        0
      }

      val bDigit = if (bStack.nonEmpty) {
        bStack.pop().toInt
      } else {
        0
      }

      var subtract = aDigit - bDigit - carry
      if (subtract < 0) {
        subtract += 10
        carry = 1
      }
      res += subtract.toString.charAt(0)
    }
    if (carry == 1) res.insert(0, "-")

    res.toString()
  }

  def shrinkZeros(a: String) : String= {
    val res = new StringBuilder();
    for (i<- a) {
      if (i != '0')
      res += i
    }
    if (res.isEmpty) res += '0'

    res.toString()
  }


  def multiply(a: String, b: String): String = {
    //repeated addition
    var btemp = b
    var mult = "0"
    while(true) {
      btemp = shrinkZeros(btemp)
      if (btemp == "0") {
        return mult
      }
      btemp = sub(btemp, "1")
      mult = sum(mult, a)
    }

    return ""
  }

}
