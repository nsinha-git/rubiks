package org.nsinha
package maxflow

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/***       b
 *       3   4
 *    a        c--5-d
 *
 *
 *
 *
 */

case class Node(n: String)
case class Edge(forwardC: Int, start: String, end: String) {
  var forward = 0
  var startNode: Node = _
  var endNode: Node = _

  override def toString: String = {
    s"Edge(forwardC=${forwardC}, start=${start}, end=${end}, forward=${forward})"
  }
}
case class Graph(nodes: List[Node], edges: List[Edge]) {
  import MaxFlowMinCut._
  val nodeMap = nodes.map(x => x.n -> x).toMap //name to Node
  val edgeMap =  mutable.HashMap[(Node, Node) , Edge]() //both way
  val nbrs = mutable.HashMap[Node, ListBuffer[Node]]()

  for (e <- edges) {
    e.startNode = getNodeFromName(e.start, this)
    e.endNode = getNodeFromName(e.end, this)
    if (!nbrs.contains(e.startNode)) {
      nbrs(e.startNode) = ListBuffer()
    }
    if (!nbrs.contains(e.endNode)) {
      nbrs(e.endNode) = ListBuffer()
    }
    nbrs(e.startNode) += e.endNode
    nbrs(e.endNode) += e.startNode
    edgeMap((e.startNode, e.endNode)) = e
    edgeMap((e.endNode, e.startNode)) = e


  }

  def printEdgeMap(path: ListBuffer[Node]) = {
    if (path == null) {
      edgeMap.foreach(edge => println(s"node:${edge._1._1}---to--->node:${edge._1._2} has ${edge._2}"))
    } else {
      var prevNode = path.head
      for (nextNode <- path.tail) {
        val edge = edgeMap((prevNode, nextNode))
        println(s"edge=${edge}")
        prevNode = nextNode
      }
    }
  }

  def findPath(s: Node, d: Node, visited: mutable.Set[Node] = mutable.Set[Node]()): ListBuffer[Node] = {
    for (n <- nbrs(s) if !visited.contains(n)) {
      if ((n == d) && (canBeAdded(edgeMap(s, n), s)(0))) {
        return ListBuffer(s,n)
      }
    }

    for (n <- nbrs(s) if !visited.contains(n)) {
      if ((canBeAdded(edgeMap(s, n), s)(0))) {
        val res = findPath(n, d, visited += s)
        visited -= s
        if (res == null) {

        } else {
          res.insert(0, s)
          return res
        }

      }
    }
    null
  }

  def findMaxFlow(path: ListBuffer[Node]): Int = {
    //given the path find the min cut
    var minT = -1
    var prevNode: Node = path.head
    for (node <- path.tail) {
      val t = canBeAdded(edgeMap(prevNode, node), prevNode)(1)
      if (minT == -1) {
        minT = t
      } else {
        minT = math.min(t, minT)
      }
      prevNode = node
    }
    minT
  }

  def update(path: ListBuffer[Node], flow: Int) = {
    var prevNode: Node = path.head
    for (node <- path.tail) {
      updateEdge(edgeMap(prevNode, node), prevNode, flow)
      prevNode = node
    }
  }


  def updateEdge(edge: Edge, from: Node, flow: Int): Unit = {
    if (edge.startNode == from) {
      edge.forward += flow
    } else { // backward flow
      edge.forward -= flow

    }
  }


  def canBeAdded(edge: Edge, from: Node): (Boolean, Int) = {
    if (edge.startNode == from) {
      if (edge.forward < edge.forwardC) {
        (true, edge.forwardC - edge.forward)
      } else {
        (false, 0)
      }
    } else {
      if (edge.forward > 0 ) {
        (true, edge.forward)
      } else {
        (false, 0)
      }
    }
  }



}
class MaxFlowMinCut(src: String, dest: String, g: Graph) {
  import MaxFlowMinCut._
  val srcNode = getNodeFromName(src, g)
  val destNode = getNodeFromName(dest, g)
  def getMaxFlow(): Int = {
    var tot = 0
    var cond = true
    while(cond) {
      val path = g.findPath(srcNode, destNode)

      if (path == null) {
        cond = false
      } else {
        val flow = g.findMaxFlow(path)
        tot += flow
        g.update(path, flow)
        println(s"path=${path} flow=${flow}")
        g.printEdgeMap(path)
        println("All edges")
        g.printEdgeMap(null)
      }
    }
    tot
  }
}


object MaxFlowMinCut {

  def getNodeFromName(n: String, g: Graph): Node = {
    g.nodeMap(n)
  }

  def main(args: Array[String]): Unit = {
    val s = Node("s")
    val v1 = Node("v1")
    val v2 = Node("v2")
    val v3 = Node("v3")
    val v4 = Node("v4")
    val t = Node("t")
    val e1 = Edge(16, "s", "v1")
    val e2 = Edge(13, "s", "v2")
    val e3 = Edge(4, "v2", "v1")
    val e4 = Edge(12, "v1", "v3")
    val e5 = Edge(9, "v3", "v2")
    val e6 = Edge(14, "v2", "v4")
    val e7 = Edge(20, "v3", "t")
    val e8 = Edge(7, "v4", "v3")
    val e9 = Edge(4, "v4", "t")
    val g = Graph(List(s,v1,v2,v3,v4,t),
      List(e1,e2,e3,e4,e5,e6,e7,e8,e9))

    val maxFlowMinCut = new MaxFlowMinCut("s", "t", g)

    println(maxFlowMinCut.getMaxFlow())

  }
}
