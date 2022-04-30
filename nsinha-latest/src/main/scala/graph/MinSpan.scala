package org.nsinha
package graph

import scala.collection.mutable

object MinSpan {

  def findMinSpanTreeKruskal(g: Graph) = {
    //start from edge
    val nodesVisited = mutable.HashSet[Node]()
    val candidatEdges = mutable.HashSet[Edge]()
    for (e <- g.edges) {
      if (nodesVisited.contains(e.n1) & nodesVisited.contains(e.n2)) {

      } else {
        candidatEdges += e
        if (!nodesVisited.contains(e.n1)) {
          nodesVisited += e.n1
        }
        if (!nodesVisited.contains(e.n2)) {
          nodesVisited += e.n2
        }
      }
    }
    Edge.genUndirectedGraph(candidatEdges.toList)

  }

  def findMinSpanTreePrim(g: Graph) = {
    //start from node
    val candidatEdges = mutable.HashSet[Edge]()
    val visitedNodes = mutable.HashSet[Node]()
    val que = mutable.Queue[Node]()
    que += g.getRandomNode()
    while (que.nonEmpty){
      val node = que.dequeue()
      if (visitedNodes.contains(node)) {

      } else {
        visitedNodes += node
        for (nbr <- node.nbrs if !visitedNodes.contains(nbr)) {
          candidatEdges += g.getEdgeBetween(node, nbr)
          que += nbr
        }
      }
    }
    Edge.genUndirectedGraph(candidatEdges.toList)

  }

}
