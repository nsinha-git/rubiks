package org.nsinha
package trees


import org.nsinha.trees.BinaryTree.{CompleteStrategy, Node, makeARandomBinTree}

import scala.collection.mutable.ListBuffer

/**
 * a binatry tree is complete if all its nodes have two children except the parent of leaf nodes such that if such a parent does not
 * have leaf node then no parent after it has leaf nodes and the parent before it also does not have leaf nodes or if it does have
 * it has full leaf nodes or only left leaf node.
 */

/**
 * numerify the bin tree
 * 0
 * 1 2
 * 3456
 * 78910
 * 11 - 13
 *
 * if any gaps say its broken
 */


object CompleteBinaryTreeChecker {
  def checkIfCompleteBinaryTree(tree: BinaryTree): Boolean = {
    val resList = ListBuffer[Int]()
    numLabelHeap(tree.rootNode, 0, resList)
    checkIfContinous(resList)
  }

  private def checkIfContinous(list: ListBuffer[Int]): Boolean = {
    val sorted = list.sorted
    for (i <- sorted.zipWithIndex) {
      if (i._1 != i._2) {
        return false
      }
    }
    true
  }
  private def getKidIndex(n: Int) = {
    (2 * n + 1, 2 * n + 2)
  }

  private def getParentIdx(n: Int): Int = {
    (n-1) >> 1
  }

  def numLabelHeap(node: Node, idx: Int, res: ListBuffer[Int]): Unit = {
    res += idx
    val (l,r) = getKidIndex(idx)
    if (node.left != null) {
      numLabelHeap(node.left, l, res)
    }
    if (node.right != null) {
      numLabelHeap(node.right, r, res)
    }
  }


  def main(args: Array[String]): Unit = {
    val tree = makeARandomBinTree(9, CompleteStrategy)
    println(checkIfCompleteBinaryTree(tree))

  }

}
