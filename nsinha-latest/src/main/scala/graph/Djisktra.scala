package org.nsinha
package graph

import scala.collection.mutable
import org.nsinha.graph.Node

/**
 * given a directed graph find the minimum path from node 1 to node2
 */
object Djisktra {
  implicit val ord: Ordering[(Int, Node)] = new Ordering[(Int, Node)] {
    def compare(x: (Int, Node), y: (Int, Node)): Int = {
      Ordering.Int.compare(x._1,y._1)
    }
  }

  def findShortestPathWeight(g: Graph, src: Node, dst: Node): Int = {
    val pq = new mutable.PriorityQueue[(Int, Node)]()
    val visited = mutable.HashSet[Node]()
    for (nbr <- src.nbrs) {
      pq += ((g.getEdgeBetween(src, nbr).wt, nbr))
    }
    visited += src

    while (pq.nonEmpty) {
      val (wt, curNode): (Int, Node) = pq.dequeue()
      if (curNode == dst) {
        return wt;
      }
      if (!visited.contains(curNode)) {
        visited += curNode
        for (nbr <- curNode.nbrs) {
          pq += ((g.getEdgeBetween(curNode, nbr).wt + wt, nbr))
        }
      } else {

      }
    }

    return -1 //not found
  }

  def main(args: Array[String]): Unit = {
    val g = Graph.createARandomDirectedGraph(5, 4, 100)

    val n1 = g.getRandomNode()
    val n2 = g.getRandomNode(n1)

    println(findShortestPathWeight(g, n1, n2))

  }

}
