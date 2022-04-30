package year2020.main
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
/*
You are trying to organize a group of skiers. The skiers are taking a trip to a large mountain, which has been rented for the day.
There are N rest points numbered from 1 to N on the mountain, and they are connected by N-1 slopes. Each slope starts at some rest point
 and goes directly to another rest point, with no intermediate slopes or rest points. A slope can be traversed in only one direction.
Each skier starts at the summit rest point and traverses a slope to reach another rest point. From there, the skier can traverse another slope
 to reach another rest point, and so on. Once a skier reaches their destination rest point, they stop skiing for the day and head to the ski
 lodge for hot cocoa. The destination rest point cannot be the summit rest point. However, notice that a skier's destination rest point can be
 the start of zero or more slopes; that is, a skier does not necessarily have to keep using available slopes until there are none available:
 they can always walk carefully down the rest of the mountain! For all rest points, there is exactly one sequence of slopes that a skier can use
 to reach it from the summit rest point.
Each slope can accommodate only a certain total number of skiers in a day, after which the snow gets too choppy to ski. In addition, the ski
resort can charge or reward each skier for each slope that they ski on. Each slope may have a different price, and each skier must pay the price for each slope they ski on. A slope's price can be positive, zero, or even negative; a negative price represents a bounty awarded for testing that slope. As the organizer, you pay all the slope prices and collect all the bounties on behalf of your group of skiers. Notice that if multiple skiers use the same slope, you pay that slope's price or collect the slope's bounty multiple times. The sum of all costs you pay minus the sum of all bounties you collect is the total expense for the trip. The expense can be positive, zero, or negative. A negative expense means that you actually made money on the trip!
As the organizer, you want to figure out the maximum number of skiers that you can put on the mountain. Also, you would like to figure out
the minimum possible expense for a trip with that maximum number of skiers.
Input
The first line of the input gives the number of test cases, T. T test cases follow. The first line of a test case contains a single integer N:
 the number of rest points on the mountain.
Each of the final N-1 lines of a test case describes a slope via four integers Ui, Vi, Si, and Ci. These are the slope's starting rest point,
 the slope's ending rest point, the maximum number of skiers the slope can accommodate, and the slope's price per skier, respectively.
The summit rest point where the skiers start from is always numbered 1.
Output
For each test case, output one line containing Case #x: y z, where x is the test case number (starting from 1), y is the maximum number of
skiers, and z is the minimum expense for having y skiers ski at least one slope each.
Limits
Time limit: 30 seconds per test set.
Memory limit: 1GB.
1 ≤ Ui ≤ N, for all i.
2 ≤ Vi ≤ N, for all i. (No slope can end at the summit rest point.)
Ui ≠ Vi, for all i.
1 ≤ Si ≤ 105, for all i.
-105 ≤ Ci ≤ 105, for all i.
There is exactly one sequence of slopes that a skier can use to reach rest point r from the summit rest point, for all r.
Test Set 1 (Visible Verdict)
1 ≤ T ≤ 100.
2 ≤ N ≤ 1000.
Test Set 2 (Hidden Verdict)
T = 17.
2 ≤ N ≤ 105.
Sample

Input

Output

2
4
1 2 2 5
1 3 2 5
3 4 1 -2
7
4 7 2 2
1 3 5 5
1 4 2 -1
3 2 3 -2
3 5 2 -1
3 6 2 2


Case #1: 4 18
Case #2: 7 15


In Sample Case #1, we can send one skier to rest point 4, one skier to rest point 3, and two skiers to rest point 2.
In Sample Case #2, we can send three skiers to rest point 2, two skiers to rest point 5, and two skiers to rest point 4.
Notice that the first slope listed in a test case does not need to start at the summit rest point, and that slopes can have Ui > Vi.
 */
/*
sol:
1. its a graph of nodes rooted at 1.
2. crete  a n+1 node with zero cost edge from all nodes  not directly connected to 1/
3. the max accomadation is summ of accomodations of direct nbs of 1,Let it be C.
Brute approach
1. for every C calculate the least cost path to n+1 from. and summ them.
For n nodes graph with M people:
M*N or N^2

Better approach:
1. While doing brute approach keep the min cnts allowed and reduce C by that much while reducing the paths.
Even better :
using dfs pre create the cost as we visit the same prefix path multiple times and changing the last mile only.
traverse the tree in beginning and create data structures. The tricky part is data structures so that code can be
simple.
 */



/**
 * slope: (node1, node2, max tries, cost)
 * @param testNo
 * @param numSlopes
 * @param slopes
 */
case class TestOne(numNode: Int, slopes: Array[(Int,Int,Int,Int)]) {
  var runningCost = 0
  var maxPop = 0
  val nodeToNodeTriesLeft = mutable.HashMap[(Int,Int), Int]()
  val nodeToNodeCost = mutable.HashMap[(Int, Int), Int]()


  /**
   *
   * src,dest,max pop,cost
   **/
  def processTest() = {
    //create inital cost and tries map
    for (sl <- slopes) {
      nodeToNodeTriesLeft((sl._1, sl._2))= sl._3
      nodeToNodeCost((sl._1,sl._2)) = sl._4
    }
    //add a dest node n+1
    for (node <- Range(1, numNode)) {
      nodeToNodeTriesLeft((node, numNode)) = 32767 //as many tries
      nodeToNodeCost((node, numNode)) = 0
    }

    maxPop = totalTries(0)

    graphCalcCost(maxPop)

    println(maxPop + ":" + runningCost)
  }


  def totalTries(startNode: Int) = {
    var tries = 0
    for (node <- Range(1, numNode)) {
      if (nodeToNodeCost.contains((startNode, node))) {
        tries += nodeToNodeTriesLeft((startNode, node))
      }
    }
    tries
  }

  /**
   * node graph with cost from one node to its neighbor.
   * Calc min cost from 1 to all
   */
  def graphCalcCost(maxPop: Int): Int = {
    var pop = maxPop
    while (pop > 0) {
      val (cost, path) = calcMinPathCost()
      val numAllowed = getAttMinPathCost(path)
      runningCost += cost * numAllowed
      updateMinPathCost(path, numAllowed)
      pop -= numAllowed
    }
    runningCost
  }

  def getAttMinPathCost(path: List[Int]) = {
    if (path.size >= 2) {
      var min = nodeToNodeTriesLeft((path(0), path(1)))
      var nodeFrom = -1
      for (nextNode <- path) {
        if (nodeFrom < 0) {
          nodeFrom = nextNode
        } else {
          min = math.min(min, nodeToNodeTriesLeft(nodeFrom, nextNode))
          nodeFrom = nextNode
        }
      }
      min
    } else {
      throw new RuntimeException
    }
  }

  def updateMinPathCost(path: List[Int], deduct: Int) = {
    var min =  nodeToNodeTriesLeft((path(0), path(1)))
    var nodeFrom = -1
    for (nextNode <- path) {
      if (nodeFrom < 0){
        nodeFrom = nextNode
      } else {
        nodeToNodeTriesLeft((nodeFrom, nextNode)) =  nodeToNodeTriesLeft((nodeFrom, nextNode)) - deduct
        nodeFrom = nextNode
      }
    }
  }

  def calcMinPathCost() = {
    implicit val ord =  new Ordering[(Int, Int)] {

      override def compare(x: (Int, Int), y: (Int, Int)): Int =  {
        if (x._1 < y._1)  {
          1
        } else if (x._1 >y._1) {
          -1
        } else {
          0
        }
      }
    }
    //pq of wt, node
    val pq = new mutable.PriorityQueue[(Int, Int)]()
    //keeps history of a node how it was visited
    val nodePath = mutable.HashMap[Int , List[Int]]()
    val nodePathCost =  mutable.HashMap[Int , Int]()

    pq +=((0, 0))
    var found = false
    var costAtTarget  = 0
    val visited = mutable.Set[Int]()

    while (pq.size > 0 & found == false) {
      val (cost, processingNode) = pq.dequeue()
      for (i <- Range(0, numNode + 1)) {
        if (nodeToNodeCost.contains((processingNode, i))) {
          val nodeVisitCost = nodeToNodeCost((processingNode, i))
          if (nodeToNodeTriesLeft((processingNode, i)) > 0) {
            pq += ((cost + nodeVisitCost, i))
            if (nodePathCost.contains(i)) {
              if (nodePathCost(i) > (cost + nodeVisitCost)) {
                nodePath(i) = nodePath.get(processingNode).getOrElse(List()) ++ List(processingNode)
                nodePathCost(i) = cost + nodeVisitCost
              }
            } else {
              nodePath(i) = nodePath.get(processingNode).getOrElse(List()) ++ List(processingNode)
              nodePathCost(i) = cost + nodeVisitCost

            }
          }
        }
      }
    }
    (nodePathCost(numNode), nodePath(numNode) ++ List(numNode))
  }
  
}

object TestOne extends App {

  val tesOne = {
    (4, Array( (0,1,2,5),( 0,2,2,5),( 2,3,1,-2)))
  }

  val testTwo = {
    (7, Array((3, 6, 2, 2), (0, 2, 5, 5), (0, 3, 2, -1), (2, 1, 3, -2), (2, 4, 2, -1), (2, 5, 2, 2) ))
  }

  //TestOne(tesOne._1, tesOne._2).processTest()
  TestOne(testTwo._1, testTwo._2).processTest()



}
