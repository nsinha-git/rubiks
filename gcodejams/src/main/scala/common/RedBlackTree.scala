package common

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 *
 */
class Node {
  var value: Integer = 0
  var left: Node = null
  var right: Node = null
  var lh = 0
  var rh = 0
}

/**
 * a rb tree uses a boolean instead of ints to track left and right height.
 * it tries to balance tree in ratio 1:2
 * how can a boolena help here:
 * 1. Lets a b color means that it should be accounted by its parent and its already balanced.
 * 2. Lets a r color mean that it itself is balanced and no further action is needed
 * 3.i.e when a b transition happens the parents must be updated. The chances of b transition is 1/2. So the height ratio of
 * tree is 2:1 at worst.
 * 4. chance of b transition is 1/2 because for black node we can add 2 reds on average. so 1b,2r,4b, 8r
 * so running average 0, 2/3,2/7, 10/15,...
 *
 */
class RedBlackTree {
  class Node {
  }

}

/***
 * One K tree balances itself when abs(loghr/hl) factor is above logK > 0.
 *
 */
class OneKtree(k:Integer) {



}

object OneKtree extends App {
  /**
   *
   * @param v
   * @param rt
   * @return
   */
  def addValue(v: Integer, rt: Node = null, k: Int): (Node, Integer) = {
    if (rt == null) {
      val node = new Node
      node.value =  v
      node.lh = 0
      node.rh = 0
      (node, 1)
    } else {
      if (rt.value > v) {
        var res = addValue(v, rt.left, k)
        rt.left = res._1
        rt.lh = res._2 + 1
        res = if (rt.lh > rt.rh && rt.lh/rt.rh > k) {
          balance(rt)
        } else if (rt.rh > rt.lh && rt.rh/rt.lh > k){
          balance(rt)
        } else {
          res
        }
        res
      } else if (rt.value < v) {
        var res = addValue(v, rt.right, k)
        rt.right = res._1
        rt.rh = res._2 + 1
        res = if (rt.lh > rt.rh && rt.lh/rt.rh > k) {
          balance(rt)
        } else if (rt.rh > rt.lh && rt.rh/rt.lh > k){
          balance(rt)
        } else {
          res
        }

        res
      } else {
        (rt, math.max(rt.lh, rt.rh))
      }
    }

  }


  /**
   * balance returns the root node of a subtree that was balanced by rotation.
   * @param node the current sub tree before rotation.
   * @return
   */
  def balance(node: Node): (Node, Integer) = {
    val prevl = node.lh
    val prevr = node.rh
    if (math.abs(prevl - prevr) < 2) {
      (node, math.max(node.lh, node.rh))
    } else if (prevl > (prevr + 1)) {
      //send root to right
      val a = node.left
      val b = node.right

      //a is root
      node.left = a.right
      node.lh = a.rh
      a.right = node
      a.rh = 1 + math.max(node.lh, node.rh)
      (a, math.max(a.lh, a.rh))
    } else if(prevr > (prevl + 1)) {
      //send root to left
      val a = node.left
      val b = node.right

      //b is root
      node.right = b.left
      node.rh = b.lh
      b.left = node
      b.lh = 1 + math.max(node.lh, node.rh)
      (b, math.max(b.lh, b.rh))

    } else {
      (node, math.max(node.lh, node.rh))
    }
  }

  /**printnode is done printing using level by level system
   * 1, use array of size 2^level for each level. create a level 0 with root.
   * 2. As soon as start processing level i create level i+1 in array
   * 3.when processing kth el in i-level array:left kid goes to 2k and rt kid to 2k+1 in i+1 level
   * 4.remove the last empty level array
   * 5.print the level array. for null elements create spaces of max digits.
   *
   *
   * @param node

   * @return
   */

  def printNodeToArrayInternal(levels:mutable.ListBuffer[Array[(Int, Node)]], max : Int): Int = {
    val curArray = levels.last
    val sz = curArray.length
    val kidsArray = levels.last
    var filled = false
    var nextMax = max
    for (i <- Range(0, sz)) {
      val processingNode = curArray(i)._2
      if (processingNode.left != null) {
        kidsArray(2 * i) = (processingNode.left.value, processingNode.left)
        nextMax = Math.max(nextMax, processingNode.left.value)
        filled = true
      }
      if (processingNode.right != null) {
        kidsArray(2 * i + 1) = (processingNode.right.value, processingNode.right)
        nextMax = Math.max(nextMax, processingNode.right.value)
        filled = true
      }
    }
    if (filled) {
      printNodeToArrayInternal(levels, nextMax)
    } else {
      levels.slice(0, levels.size - 1)
      nextMax
    }
  }

  def printNodeToArray(node: Node) = {
    if (node != null) {
      val levelArray = mutable.ListBuffer[Array[(Integer, Node)]]()
      levelArray.last(0) = (node.value, node)
      val maxValue = 0//printNodeToArrayInternal(levelArray, node.value)
      val digits = math.ceil(math.log10(maxValue)).toInt
      val processedArray = mutable.ListBuffer[Array[Integer]]()
      val midElemIndx = levelArray.last.size/2
      for (i <- Range(0, levelArray.length)) {
        processedArray += new Array[Integer](levelArray.last.size)
        if (i != 0) {
          for (zpValue <- levelArray(i).zipWithIndex) {
            processedArray.last(zpValue._2 + midElemIndx - levelArray(i).length/2) = zpValue._1._1
          }
        } else { //i =0
          processedArray.last(0) = levelArray(i)(0)._1
        }
      }
      printFirstElem(processedArray(0)(0), digits, midElemIndx)
      printRemainingArrays(processedArray.tail, digits)

    }
  }

  def printToDigit(i: Integer, d: Int): Unit = {
    d match {
      case 2 => printf("%2d", i)
      case 3 => printf("%3d", i)
      case 4 => printf("%4d", i)
      case 5 => printf("%5d", i)
      case 6 => printf("%6d", i)
      case 7 => printf("%7d", i)
      case 8 => printf("%8d", i)
    }
  }

  def printFirstElem(firstEl: Integer, d: Int, midIndx: Int): Unit = {
    for (j <- Range(0, midIndx)) {
      Range(0,d).foreach(_ => print(" "))
    }
    printToDigit(firstEl, d)
  }

  def printRemainingArrays(buffer: ListBuffer[Array[Integer]], d: Int) = {
    for (ar <- buffer)  {
      for (el <- ar) {
        if (el == null) {
          Range(0,d).foreach(_ => print(" "))
        } else {
          printToDigit(el, d)
        }
      }
    }

  }



  def testBalance() = {
    val a = new Node()
    a.value = 1
    val b = new Node()
    b.value = 2
    val c = new Node()
    c.value = 3
    val d = new Node()
    d.value = 4
    val e = new Node()
    e.value = 5

    a.left = b
    a.right = c
    c.left = d
    d.left = e
    e.lh = 0
    e.rh = 0
    d.lh = 1
    d.rh = 0
    c.lh = 2
    c.rh = 0
    a.rh = 3
    a.lh = 1
    b.lh = 0
    b.rh = 0
    val (testRoot, h) = balance(a)
    printNodeToArray(testRoot)
  }

  def testAddNodes(): Unit = {
    val k = 2

    var rt = (addValue(3,  null, k)) ._1
    rt = addValue(1, rt, k)._1
    rt = addValue(4, rt, k)._1
    rt = addValue(0, rt, k)._1
    rt = addValue(2, rt, k)._1
    rt = addValue(2, rt, k)._1
    rt = addValue(5, rt, k)._1
    rt = addValue(6, rt, k)._1
    rt = addValue(7, rt, k)._1
  }

  testBalance()

}
