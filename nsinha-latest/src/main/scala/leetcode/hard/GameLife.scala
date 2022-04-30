package org.nsinha
package leetcode.hard

import scala.util.Random

object GameLife {


  def generateMatrix(n: Int, m: Int, genVal: () => Boolean): Array[Array[Boolean]] = {
    val res = new Array[Array[Boolean]](n)
    for (i <- Range(0, n)) {
      res(i) = new Array[Boolean](m)
    }

    for (i <- Range(0, n)) {
      for (j <- Range(0, m)) {
        res(i)(j) = genVal()
      }
    }
    res
  }


  def runPhase(ar: Array[Array[Boolean]]): Unit = {
    val i = Random.between(0, ar.length)
    val j = Random.between(0, ar.length)
    val newVal = eval(get(ar, i, j), get(ar, i, j - 1), get(ar, i - 1, j - 1), get(ar, i - 1, j))
    if (newVal != ar(i)(j)) {
      //println(s"changed ${i},${j}")
    }
    ar(i)(j) = newVal
  }

  def get(array: Array[Array[Boolean]], i: Int, j: Int): Boolean = {
    if (i < 0 || i >= array.length) {
      false
    } else if (j < 0 || j >= array(i).length) {
      false
    } else {
      array(i)(j)
    }

  }

  def toInt(p: Boolean): Int =
    if (p) 1 else 0

  def eval(v1: Boolean, v2: Boolean, v3: Boolean, v4: Boolean): Boolean = {
    val t = toInt(v1) + toInt(v2) + toInt(v3) + toInt(v4)

    if (t > 2) {
      true
    } else if (t < 2) {
      false
    } else {
      v1
    }
  }

  def main(args: Array[String]): Unit = {
    val ar = generateMatrix(10,10, util.Random.nextBoolean)

    for (i <- Range(0, ar.length)) {
      println(ar(i).toList)
    }
    for (phase <- Range(0, 1000)) {

      runPhase(ar)
    }
    println()
    println()

    for (i <- Range(0, ar.length)) {
      println(ar(i).toList)
    }

  }










}
