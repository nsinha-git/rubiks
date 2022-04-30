package org.nsinha
package leetcode.hard

import scala.collection.mutable

/**
 * Binary Tree Maximum Path Sum
 * find the max path betwen nodes in a binary tree
 */

/**
 * let [1,2,3,4,5,6,7,8]
 * consider a path P rooted at R.
 * any node(n) can have two children. We can also store best weights of left and right on each children.
 * Then max(lw+rw) is the node.
 */
class BinTree {
  var root: BinTreeNode = _
}

class BinTreeNode {
  var l: BinTreeNode = _
  var r: BinTreeNode = _
  var v: Int = 0
}
class MaxPathSumBinTree {
  val nodeMaxValuesLeft = mutable.HashMap[BinTreeNode, Int]()
  val nodeMaxValuesRight = mutable.HashMap[BinTreeNode, Int]()
  val nodeMaxLeafLeft = mutable.HashMap[BinTreeNode, BinTreeNode]()
  val nodeMaxLeafRight = mutable.HashMap[BinTreeNode,BinTreeNode]()

  def findMaxPathOfBinTree(tree:BinTree): (Int, Int) = {
    var max = 0
    var maxKey: BinTreeNode = null
    val rootV = traverseNode(tree.root)
    for (key <- nodeMaxValuesLeft.keys) {
      val t = nodeMaxValuesLeft(key) + nodeMaxValuesRight(key)
      if (t > max) {
        max = t
        maxKey = key
      }
    }
    (nodeMaxLeafLeft(maxKey).v, nodeMaxLeafRight(maxKey).v)
  }

  def traverseNode(node: BinTreeNode): (Integer, BinTreeNode) = {
    if (node == null) {
      return (null, null)
    }
    if (node.l == null && node.r == null) {
     return (node.v, node)
    }
    val lv = traverseNode(node.l)
    val rv = traverseNode(node.r)
    if (lv != null) {
      nodeMaxValuesLeft(node) = lv._1
      nodeMaxLeafLeft(node) = lv._2
    }
    if (rv != null) {
      nodeMaxValuesRight(node) = rv._1
      nodeMaxLeafRight(node) = rv._2
    }
    if (lv!=null && rv!=null) {
      if (lv._1 > rv._1) {
        (lv._1 + node.v, lv._2)
      } else {
        (rv._1 + node.v, rv._2)
      }
    } else if(lv!=null){
      (lv._1 + node.v, lv._2)
    } else {
      (rv._1 + node.v, rv._2)
    }
  }
}
