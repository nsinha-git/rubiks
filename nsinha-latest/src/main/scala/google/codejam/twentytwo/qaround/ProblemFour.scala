package org.nsinha
package google.codejam.twentytwo.qaround

import scala.collection.mutable

/***
 * A tree graph with one node point to abyss(null).
 * The node(s) that no oone points to are roots.
 * Permute the roots to find max path.
 *
 *
 */

/**
 *We can write a max flow look alike graph traversal using a hash map and graph traversal.
 *Let q1 be root nodes queue
 * 1. Dequue q1, call it r.
 * 2. Find a path Pr(defined by edges labels.edge labels from a node r is named Er) and value of each edge label(map).
 *    2a. if visited edgemap contains the edge then dont count the edge in Pr to obtain the value of path else use it.
 *    2b.keep the list of unused path edges U.
 *    2c. if the value of path could have been increased by using the highest of U, the check vsited edge map of U and get path p-U-old
 *    2d. See if p-u-old can remove U w/o affect its value. if yes update edge map of U with pr and update value of pr.
 *    2c. in visited Edgemap store each edge to its path Pr which contributed and dont have entry.
 * 3.
 */
class ProblemFour {
  implicit val ordering: Ordering[(Int,Int)] = new Ordering[(Int, Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = Ordering.Int.compare(x._1, y._1)
  }
  val nbrs = mutable.HashMap[Int, Int]()
  val values = mutable.HashMap[Int, Int]()
  var allNodes: Set[Int] = null
  var roots:Set[Int] = null

  /***properties of paths**/
  val visitedNodeMap =  mutable.HashMap[Int, Set[Int]]()
  val pathValue = mutable.HashMap[Set[Int], Int]()

  def createGraph(value: Array[Int], conn: Array[Int]): Unit = {
    for (v <- value.zipWithIndex) {
      values(v._2 + 1) = v._1
      nbrs(v._2 + 1) = conn(v._2)
    }
    allNodes=Range(1, value.length+1).toSet
    roots = getRoots()
  }


  def getRoots(): Set[Int] = {
    allNodes.diff(nbrs.values.toSet)
  }


  def findVBestPathsToZero(): Int = {
    for (r <- roots) {
      val pathInProgress = mutable.HashSet[Int]() //node, path value
      pathInProgress += r
      var pathValueInProgress = values(r)
      var cond = true
      var nextNode = r
      val unusedNodes = mutable.PriorityQueue[(Int, Int)]()
      while(cond) {
        val nbr = nbrs(nextNode)
        if(nbr == 0) {
          cond = false
          val path = pathInProgress.toSet
          //set new nodes paths. we will deal with used nodes later
          path.filter(!visitedNodeMap.contains(_)).foreach(x => visitedNodeMap(x) = path)
          //check if the path value could have been increased w/o affecting other previous paths
          val unusedNodes_preserved = unusedNodes.map(x => x._2).toSet
          while (unusedNodes.nonEmpty) {
            val (valueOfNode,unusedNode) = unusedNodes.dequeue()
            if (valueOfNode > pathValueInProgress) { //could be improved
              val prevPath = visitedNodeMap(unusedNode)
              val prevPathValue = pathValue(prevPath)
              //check if any node in prevPath also had a equal value
              if (checkIfWeCanSwitchPathOfThisNode(prevPath, valueOfNode- pathValueInProgress, unusedNode)) {
                reeval(prevPath, unusedNode)
              //if (prevPath.filter(x => x!=unusedNode).filter(x => values(x) == valueOfNode).nonEmpty) {
                pathValueInProgress = valueOfNode
                //we can remove usedNode from old path and switch to new path
                //also we can set it for remaining unused nodes also as they will be shared path
                for (remUnusedNode <- unusedNodes_preserved) {
                  visitedNodeMap(remUnusedNode) = path
                }
                unusedNodes.clear() //break the while loop
              }
            }

          }
          val pathValueFinal = pathValueInProgress
          pathValue(path) = pathValueFinal
        } else {
          if (!visitedNodeMap.contains(nbr)){
            pathInProgress += nbr
            pathValueInProgress = math.max(pathValueInProgress, values(nbr))
            nextNode = nbr
          } else {//this nbr has already been part of some other path
            unusedNodes += ((values(nbr), nbr))
            pathInProgress += nbr
            nextNode = nbr
          }
        }
      }
    }
    pathValue.values.sum
  }


  def checkIfWeCanSwitchPathOfThisNode(prevPath: Set[Int], diff: Int, nodeLost: Int) : Boolean = {
    val newPathValue = prevPath.filter(_!= nodeLost).map(x => values(x)).max
    val oldPathValue = pathValue(prevPath)
    if ((oldPathValue - newPathValue) < diff) {
      true
    } else {
      false
    }
  }

  def reeval(prevPath: Set[Int], nodeLost: Int): Unit = {
    val newPathValue = prevPath.filter(_!= nodeLost).map(x => values(x)).max
    pathValue(prevPath) = newPathValue
  }


}

object ProblemFour {

  def main(args: Array[String]): Unit = {
    val problemFour = new ProblemFour()
    problemFour.createGraph(Array(100, 100, 100, 90, 80, 100, 90, 100), Array(0, 1, 2, 1, 2, 3, 1, 3))
    println(problemFour.findVBestPathsToZero())
  }

}
