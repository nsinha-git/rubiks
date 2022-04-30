package org.nsinha
package trees

import org.nsinha.trees.BinaryTree.Node

class BinaryTree {
  var rootNode: Node = _
}

object BinaryTree {
  class Node(v: Int) {
    var left: Node = _
    var right: Node = _
    var value: Integer = v
  }

  def makeARandomBinTree(sz: Int, startegy: Strategy): BinaryTree = {
    if (sz <=0) {
      null
    } else {
      val startVal = 0
      val c = new BinaryTree()
      c.rootNode = new Node(startVal)
      insertKids(c.rootNode, sz-1, startVal + 1, startegy)
      c
    }

  }

  class Strategy()
  object RandomKidStartegy extends Strategy
  object BalancedSiblingStartegy extends Strategy
  object CompleteStrategy extends Strategy



  def insertKids(node: BinaryTree.Node, sz: Int, startVal: Int = 0, strategy: Strategy=CompleteStrategy): Int = {
    var startValLocal = startVal
    if (sz > 2) {
      val split: Int = strategy match {
        case RandomKidStartegy =>
          //split sz into two parts and insert firt part in left kid and rest in right kid
          util.Random.between(0, sz-2)
        case BalancedSiblingStartegy =>
          (sz-2) / 2
        case CompleteStrategy =>
          val t: Int = (sz-2)/2
          if( 2 * t < (sz-2)) {
            t + 1
          } else {
            t
          }

      }
      node.left = new Node(startValLocal)
      startValLocal +=1
      node.right = new Node(startValLocal + 1)
      startValLocal +=1
      if (sz -2 <= 2) {
        startValLocal = insertKids(node.left, sz -2 , startValLocal,  strategy)
      } else {
        startValLocal = insertKids(node.left, split, startValLocal,  strategy)
        startValLocal = insertKids(node.right, sz - 2 - split, startValLocal, strategy)
      }
      startValLocal
    } else {
      if (sz == 1) {
        if (strategy == CompleteStrategy) {
          node.left = new Node(startVal)
        } else {
          if (util.Random.nextBoolean()) {
            node.left = new Node(startVal)
          } else {
            node.right = new Node(startVal)
          }
        }
        startVal + 1
      } else if(sz == 2) {
        node.left = new Node(startVal)
        node.right = new Node(startVal + 1)
        startVal + 2
      } else {
        startVal
      }
    }
  }



}
