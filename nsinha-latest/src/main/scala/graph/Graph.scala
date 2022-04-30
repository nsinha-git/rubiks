package org.nsinha
package graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

case class Graph(nodes: List[Node], edges: Set[Edge]) {
  val mapOfNodesToEdges = mapNodesToEdges()

  def mapNodesToEdges(): Map[(Node, Node), Edge] = {
   edges.map {e =>
      (e.n1,e.n2) -> e
    } toMap
  }

  def getRandomNode(n: Node = null): Node = {
    if (n != null) {
      val idx = util.Random.between(0, nodes.length - 1)
      nodes.filter(_ != n)(idx)
    } else {
      val idx = util.Random.between(0, nodes.length)
      nodes.filter(_ != n)(idx)
    }
  }

  def getEdgeBetween(n1: Node, n2: Node): Edge = {
    mapOfNodesToEdges((n1,n2))
  }

}

class Node(v_ : Int = 0, nbrs_ : List[Node] = List.empty) {
  var v: Int = v_
  var nbrs = nbrs_

  def genUnDirectedEdges(edgeWtMax: Int = 0): List[Edge] = {
    nbrs.flatMap(n =>  {
      val wt = util.Random.between(0, edgeWtMax)
      List(Edge(this, n, wt), Edge(n, this, wt))
    })
  }

  def genDirectedEdges(edgeWtMax: Int): List[Edge] = {
    nbrs.flatMap(n => List(Edge(this, n, util.Random.between(0, edgeWtMax))))
  }

}
class Edge(val n1: Node, val n2: Node, val wt : Int = 0)

object Node {
  def generateEdges(nodes: List[Node]): List[Edge] = {
    nodes.flatMap(n => n.genUnDirectedEdges())
  }
}
object Edge {
  def revrseEdge(e: Edge) = {
    Edge(e.n2, e.n1, e.wt)
  }

  def genUndirectedGraph(edges: List[Edge]): Graph = {
    val visited = mutable.HashSet[Edge]()
    val nodes = mutable.HashSet[Node]()

    for (e <- edges if !visited.contains(e)) {
      visited += e
      visited += revrseEdge(e)
      nodes += e.n1
      nodes += e.n2
    }

    Graph(nodes.toList, edges.toSet)
  }

  def genDirectedGraph(edges: List[Edge]): Graph = {
    val visited = mutable.HashSet[Edge]()
    val nodes = mutable.HashSet[Node]()

    for (e <- edges if !visited.contains(e)) {
      visited += e
      nodes += e.n1
      nodes += e.n2
    }

    Graph(nodes.toList, edges.toSet)
  }

}

object Graph {

  def createARandomDirectedGraph(sz: Int, maxConnectivity: Int, edgeWtMax: Int): Graph = {
    val intToNode = mutable.HashMap[Int, Node]()
    for (i <- Range(0, sz)) {
      val node = new Node(i)
      intToNode(i) = node
    }

    for (i <- Range(0, sz)) {
      val conn = util.Random.between(0, maxConnectivity)
      val nbrs = Range(0, conn).map(j => util.Random.between(0, sz)).map(intToNode(_)).filter(_ != intToNode(i)) toList

      intToNode(i).nbrs = nbrs
    }

    val allNodes = intToNode.map(x => x._2).toList
    val allEdges = allNodes.flatMap(n => n.genDirectedEdges(edgeWtMax))
    Graph(allNodes, allEdges.toSet)
  }

  def createARandomUnDirectedGraph(sz: Int, maxConnectivity: Int, edgeWtMax: Int): Unit = {
    val intToNode = mutable.HashMap[Int, Node]()
    for (i <- Range(0, sz)) {
      val node = new Node(i)
      intToNode(i) = node
    }
    val edges = ListBuffer[Edge]()

    for (i <- Range(0, sz)) {
      val conn = util.Random.between(0, maxConnectivity/2)
      val nbrs = Range(0, conn).map(j => util.Random.between(0, sz)).map(intToNode(_)).filter(_ != intToNode(i)) toList


      nbrs.foreach(n => edges += Edge(intToNode(i), n, util.Random.between(0, edgeWtMax)))
    }
    Edge.genUndirectedGraph(edges.toSet.toList)

  }
}


