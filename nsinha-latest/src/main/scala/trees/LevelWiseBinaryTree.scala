package org.nsinha
package trees

import org.nsinha.trees.BinaryTree.{CompleteStrategy, Node, makeARandomBinTree}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object LevelWiseBinaryTree {

  def printLevelWise(tree: BinaryTree): Unit = {
    printLevelWise(ListBuffer(tree.rootNode), ListBuffer(), 0)
  }

  def printLevelWiseReverse(tree: BinaryTree): Unit = {
    printLevelWiseReverse(ListBuffer(tree.rootNode), ListBuffer(), 0, false)
  }

  def printLevelWise(levelOne: ListBuffer[Node], levelTwo: ListBuffer[Node], curProcessing: Int = 0): Unit = {
    if (levelTwo.isEmpty && levelOne.isEmpty) {
      return
    }
    if (curProcessing == 0) {
      for (n <- levelOne) {
        if (n.left != null) levelTwo += n.left
        if (n.right != null) levelTwo += n.right
        print((n.value))
      }
      println()
      println("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
      levelOne.clear()
      printLevelWise(levelOne, levelTwo, 1)
    } else {
      for (n <- levelTwo) {
        if (n.left != null) levelOne += n.left
        if (n.right != null) levelOne += n.right
        print(n.value)
      }
      println()
      println("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
      levelTwo.clear()
      printLevelWise(levelOne, levelTwo, 0)
    }
  }

  def printLevelWiseReverse(levelOne: ListBuffer[Node], levelTwo: ListBuffer[Node], curProcessing: Int = 0, rev: Boolean): Unit = {
    if (levelTwo.isEmpty && levelOne.isEmpty) {
      return
    }
    val stack = mutable.Stack[Int]()
    if (curProcessing == 0) {
      for (n <- levelOne) {
        if (n.left != null) levelTwo += n.left
        if (n.right != null) levelTwo += n.right
        if (!rev) {
          print((n.value))
        } else {
          stack.push(n.value)
        }
      }
      if(rev) {
        for (t <- stack) {
          print(t)
        }
      }
      println()
      println("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
      levelOne.clear()
      printLevelWiseReverse(levelOne, levelTwo, 1, !rev)
    } else {
      for (n <- levelTwo) {
        if (n.left != null) levelOne += n.left
        if (n.right != null) levelOne += n.right
        if (!rev) {
          print((n.value))
        } else {
          stack.push(n.value)
        }
      }
      if(rev) {
        for (t <- stack) {
          print(t)
        }
      }
      println()
      println("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
      levelTwo.clear()
      printLevelWiseReverse(levelOne, levelTwo, 0, !rev)
    }
  }


  def connectLevelLinks(tree: BinaryTree): Map[Node, Node] = {
    val res = mutable.HashMap[Node, Node]()
    connectLevelLinksNode(ListBuffer(tree.rootNode),ListBuffer[Node](), null, 0, res)
    res.toMap
  }

  def connectLevelLinksNode(levelOne: ListBuffer[Node], levelTwo: ListBuffer[Node], lastNode_ : Node, curProcessing: Int = 0,
                        map: mutable.HashMap[Node, Node]): Unit = {
    var lastNode = lastNode_
    if (levelTwo.isEmpty && levelOne.isEmpty) {
      if (lastNode!= null) {
        map(lastNode) = null
      }
      return
    }
    if (curProcessing == 0) {
      for (n <- levelOne) {
        if (lastNode != null)
          map(lastNode) = n
        if (n.left != null) levelTwo += n.left
        if (n.right != null) levelTwo += n.right
        lastNode = n
      }
      levelOne.clear()
      connectLevelLinksNode(levelOne, levelTwo, lastNode, 1, map)
    } else {
      for (n <- levelTwo) {
        if (lastNode != null)
          map(lastNode) = n
        if (n.left != null) levelOne += n.left
        if (n.right != null) levelOne += n.right
        lastNode = n
      }
      levelTwo.clear()
      connectLevelLinksNode(levelOne, levelTwo, lastNode, 0, map)
    }

  }


  def main(args: Array[String]): Unit = {
    val tree = makeARandomBinTree(9, CompleteStrategy)
    println(connectLevelLinks(tree))
    printLevelWise(tree)
    printLevelWiseReverse(tree)
  }
}

