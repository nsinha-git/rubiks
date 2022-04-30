package common

import scala.collection.mutable

case class MaxFlow(edgeWt: Map[(Int,Int), Int], src: Int, dest: Int, numNodes: Int) {
  def doMaxFlow() = {
    val forwardPath = mutable.Map[(Int, Int), Int]() ++ edgeWt.toSeq
    val backwardPath = createBackwardPath(forwardPath toMap)
    var cond = true
    var mxFlow = 0
    while (cond) {
      val extraFlow = findFlowOnCurrGraph(forwardPath, backwardPath)
      if (extraFlow == 0) {
        cond = false
      }
      mxFlow += extraFlow
    }
    mxFlow
  }

  def createBackwardPath(forwardPath: Map[(Int, Int), Int]) = {
    val backwardPath = mutable.Map[(Int, Int), Int]()
    for (i <- forwardPath) {
      backwardPath((i._1._2, i._1._1)) = 0
    }
    backwardPath
  }

  def findFlowOnCurrGraph(forwardPath: mutable.Map[(Int, Int), Int], backwardPath: mutable.Map[(Int, Int), Int]) = {
    val path = findPath(forwardPath, backwardPath)
    val mxValuePath = getMinOfPath(forwardPath, backwardPath, path)
    updatePath(forwardPath, backwardPath, path, mxValuePath)
    mxValuePath
  }


  def findPath(forwardPath: mutable.Map[(Int, Int), Int], backwardPath: mutable.Map[(Int, Int), Int]): List[Int] = {
    val visited = mutable.HashSet[Int](src)
    val pathMap = mutable.HashMap[Int, List[Int]]()
    var found = false
    val queue = mutable.Queue[Int](src)
    while (queue.nonEmpty && found == false) {
      val curProcessing = queue.dequeue()
      for (i <- Range(0, numNodes) if !visited.contains(i)) {
        if (forwardPath.contains((curProcessing, i))) {
          if (forwardPath((curProcessing, i)) > 0) {
            queue += i
            pathMap(i) = pathMap.get(curProcessing).getOrElse(List()) ++ List(curProcessing)
            if (i == dest) {
              found = true
            }
            visited += i
          }
        } else if (backwardPath.contains((curProcessing, i))) {
          if (backwardPath((curProcessing, i)) > 0) {
            queue += i
            pathMap(i) = pathMap.get(curProcessing).getOrElse(List()) ++ List(curProcessing)
            if (i == dest) {
              found = true
            }
            visited += i
          }
        }
      }
    }

    if (found == true) {
      pathMap(dest) ++ List(dest)
    } else {
      List.empty
    }
  }


  def getMinOfPath(forwardPath: mutable.Map[(Int, Int), Int], backwardPath: mutable.Map[(Int, Int), Int], path: List[Int]) = {
    if (path.isEmpty) {
      0
    } else {
      var startNode = path(0)
      var min = 32767
      for (nxt <- path) {
        if (nxt == startNode) {
        } else {
          min = Math.min(min, forwardPath.getOrElse(((startNode, nxt)), backwardPath((startNode, nxt))))
          startNode = nxt
        }
      }
      min
    }
  }

  def updatePath(forwardPath: mutable.Map[(Int, Int), Int], backwardPath: mutable.Map[(Int, Int), Int], path: List[Int], min: Int) = {
    if (path.nonEmpty) {
      var startNode = path(0)
      for (nxt <- path) {
        if (nxt == startNode) {
        } else {
          if (forwardPath.contains((startNode, nxt))) {
            forwardPath((startNode, nxt)) = forwardPath((startNode, nxt)) - min
            backwardPath((nxt, startNode)) = backwardPath((nxt, startNode)) + min
          } else {
            backwardPath((startNode, nxt)) = backwardPath((startNode, nxt)) - min
            forwardPath((nxt, startNode)) = forwardPath((nxt, startNode)) + min
          }
          startNode = nxt
        }
      }
    } else {

    }
  }
}

object MaxFlow extends App {

  object TestCases {
    def tcase1 = {
      (Map((0,1)->10, (0,2)->10, (1,2)->30,(1,3)->10 ,(2,3)->20 ), 0, 3, 4)
    }
  }
  import TestCases._


    val mxflow = MaxFlow(tcase1._1, tcase1._2, tcase1._3, tcase1._4)
    println(mxflow.doMaxFlow())

}