package common

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, PriorityQueue}
import scala.language.{postfixOps, reflectiveCalls}

case class Graphops (szNodes:Int, edgeMap: Map[String, List[String]], edgeWt: Map[(String, String), Int])  {
  
  val intAscending = new Ordering[Int] {
    override def compare(x: Int, y: Int): Int = {
      if (x < y ) return -1
      if (x > y ) return 1
      return 0
    }
  }

  val intDesending = new Ordering[Int] {
    override def compare(x: Int, y: Int): Int = {
      if (x < y ) return 1
      if (x > y ) return -1
      return 0
    }
  }
  //input is simple
  //a edge Map with node-[nodes]
  //a edge weight map with node-node-weight
  //a edge applicable map with edge-edge-value
  //a edge applicability fn that evalues the edge applicabe map.
  
  val NOTFOUND = "notFound"
  def doBfs[Value](R: Value, rootNode: String, nodeValueMap: Map[String, Value]): String = {
    val queue = ListBuffer[String](rootNode)
    
    while (queue.size > 0) {
      val curEl = queue.remove(0)
      val toNodes = edgeMap(curEl) 
      queue ++= toNodes
      if (nodeValueMap(curEl) == R){
        return curEl
      }
    }
    NOTFOUND
  }
  
  
  def doDfs [Value](R: Value, rootNode: String, nodeValueMap: Map[String, Value]): String = {
    if (nodeValueMap(rootNode) == R) {
      return rootNode
    } else {
      for (el <- edgeMap(rootNode)) {
        val retNode = doDfs(R, el, nodeValueMap)
        if (retNode != NOTFOUND) {
          return retNode
        } else {
          return NOTFOUND
        }
      }
    }
    return NOTFOUND
  }

  def doDfsWithPath [Value](R: Value, rootNode: String, nodeValueMap: Map[String, Value], path: List[String] = List()): (String, List[String]) = {
    val pathNew: List[String] = path :+ rootNode
    if (nodeValueMap(rootNode) == R) {
      return (rootNode, pathNew)
    } else {
      for (el <- edgeMap(rootNode)) {
        val (retNode, retPath) = doDfsWithPath(R, el, nodeValueMap, pathNew)
        if (retNode != NOTFOUND) {
          return (retNode, retPath)
        } else {
          return (NOTFOUND, pathNew)
        }
      }
    }
    return (NOTFOUND, pathNew)
  }

}
