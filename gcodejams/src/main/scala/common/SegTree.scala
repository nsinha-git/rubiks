package common

import scala.math.abs

/**aVl tree
 * 1.given pts like 0,1,2,3,  6,7,8, 11,12, etc.
 * create a segment (0,3) (6,8) (11,12)
 * when queried for 1 this returns true.
 * 2. Queries of (1,3) in tree also returns true
 * 3.Nbrhood queries like nbr(1)=(0,3)
 * -------------
 * 1.Can be built incrementally
 * --------------
 * 1. a segment is a strictly less than b is b._2 is stricty greater than a._2
 * 2. a segment with same ._2 a,b then a<b if a._1>b._1 i.e span is stricter
 * 3. All nodes are only and only from input sequences.
 *
 */
case class SegTreeNode(span:(Int, Int)) {
  var left: SegTreeNode = null
  var right: SegTreeNode = null
  var hl = 0
  var hr = 0
}

class SegTree {
  import SegTree._

  def addSeg(root: SegTreeNode, seg: (Int, Int)): SegTreeNode = {
    var root_ = root
    if (root == null) {
      SegTreeNode(seg)
    } else {
      val cmp = compare(root.span, seg)
      if (cmp == 1) {
        root.left = addSeg(root.left, seg)
        root.hl += 1
        if (root.hl - root.hr > 1) {
          root_ = avl(root)
        }
        root_
      } else if (cmp == -1) {
        root.right = addSeg(root.right, seg)
        root.hr += 1
        root
      } else { //identical
        root
      }
    }
  }

  def avl(segTreeNode: SegTreeNode): SegTreeNode = {
    null
  }




}

object SegTree extends App{

  def segs1 = Array((-1,1), (0,2), (1,3), (-3,-1), (1,10))
  def compare(seg1:(Int, Int), seg2:(Int, Int)): Int = {
    val span1 = seg1._2 - seg1._1
    val span2 = seg2._2 - seg2._1
    if (seg1._2 < seg2._2) return -1
    if (seg1._2 > seg2._2) return 1

    if (span1 < span2) {
      return -1
    } else if (span1 > span2){
      return 1
    }
    return 0
  }

  def createSegmentTree(inp: Array[(Int, Int)]): SegTreeNode = {
    var root: SegTreeNode = null
    val segTree = new SegTree()
    for (x <- inp) {
      root = segTree.addSeg(root, x)
    }
    root
  }


  def testCompare = {
    assert(compare((0,2), (0,3)) == -1)
    assert(compare((0,3), (0,2)) == 1)
    assert(compare((0,2), (0,2)) == 0)
    assert(compare((0,2), (1,2)) == 1)
    assert(compare((1,2), (0,2)) == -1)

  }

  createSegmentTree(segs1)

}
