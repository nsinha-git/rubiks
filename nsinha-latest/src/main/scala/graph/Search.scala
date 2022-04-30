package org.nsinha
package graph

import scala.collection.mutable

object BfsSearch {

  def doSearch(g: Graph, p: Node => Boolean): Node = {
    val visited = mutable.HashSet[Node]()
    for (node <- g.nodes if !visited.contains(node)) {
      val queue = mutable.Queue[Node] ()
      queue += node
      while(queue.nonEmpty) {
        val nxtNode = queue.dequeue()
        if (!visited.contains(nxtNode)) {
          if (p(nxtNode)) return nxtNode
          visited += nxtNode
          for (nbr <- nxtNode.nbrs) {
            queue += nbr
          }
        }
      }
    }
    null
  }
}

object DfsSearch {
  def doSearch(g: Graph, p: Node => Boolean): Node = {

    val visited = mutable.HashSet[Node]()

    for (node <- g.nodes if !visited.contains(node)) {
      visited += node
      val n = doSearch(node, p, visited)
      if (n != null) {
        return n
      }
    }
    null
  }

  def doSearch(n: Node, p: Node => Boolean, visited: mutable.HashSet[Node]): Node = {
    if (p(n)) {
      return n
    } else {
      for (nbr <- n.nbrs if !visited.contains(nbr)) {
        val res = doSearch(nbr, p, visited)
        if (res != null) {
          return res
        } else {

        }
      }
      null
    }
  }

}


