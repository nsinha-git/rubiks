package leetcode

//
// Given a Binary Tree's (BT) root node, detect if it forms a complete tree:
//
//   A complete BT has the following properties:
//
//      a) Either all rows are full with nodes (OR)
//      b) The last row alone may be partial but the nodes must be left aligned
//
//   Complete
// --------------------
//
//       A
//
//
//       A
//      /     alpha
//     B
//
//
//       A
//      / \     beta
//     B   C
//
//   Not complete
// --------------------
//
//       A
//        \   theta
//         B
//

///.    A
//    b.  c   L-1
//d e    f. XXXX L

// condition b:

/**1. get L
2. check if complete tree till l-1
3. Get nodes at L-1
4. Make sure b is true for L-1 nodes.

n + lg(n) + o(n)


 */
case class TreeNode(l: TreeNode, r: TreeNode)

class TreeCompleteAndLeftAligned() {


  /**
  checkLeftAlign(alpha,1) == (true,List(B))



   */




  def solve(root: TreeNode): Boolean = {

    if (root == null) return true


    val L = getMaxL(root)

    val compl = checkCompletenessAndReturnLastLevel(root, L-1)

    if (compl == null) {
      false
    } else {
      if (checkLeftAlign(compl)) return true
      false
    }

  }


  //root is never null
  def getL(root: TreeNode, depth: Int = 0): Int = {
    if (root.l == null) {
      return depth
    } else {
      getL(root.l, depth +1)
    }

  }


  //root is never null
  def getMaxL(root: TreeNode): Int = {
    var mxLeft = 0
    var mxRight = 0
    if (root.l != null) {
      mxLeft = getMaxL(root.l) + 1
    }

    if (root.r != null) {
      mxRight = getMaxL(root.r) + 1
    }

    Math.max(mxLeft, mxRight)

  }



  /*
  nodes are left to rigt ordered

  */
  def checkCompletenessAndReturnLastLevel(node: TreeNode, tillLevel: Int):  List[TreeNode] = {
    if (tillLevel == 0) {
      return List(node)
    }
    if (node.l != null && node.r != null) {
      val list_l = checkCompletenessAndReturnLastLevel(node.l, tillLevel -1)
      val list_r = checkCompletenessAndReturnLastLevel(node.r, tillLevel -1)
      if (list_l == null || list_r == null) {
        return null
      } else {
        list_l ++ list_r
      }
    } else {
      return null
    }

  }

  //assuming nodes are left ordered. check it
  def checkLeftAlign(nodes: List[TreeNode]): Boolean = {
    var cond = true//till now every node is left aligned
    var end = false
    for (node <- nodes) {
      if (cond) {
        if (!end) {
          if (node.l != null && node.r != null) {
            //pass
          } else if (node.r == null) {
            end = true //l is empty or not does not matter
          } else {//r is not null and l is null as we tested earlier and one of them is null, clearly r is not so l is!
            cond = false
          }
        } else {
          //end is seen no no more kids are expected
          if (node.l != null || node.r != null) {
            cond = false
          }
        }
      } else {
          //cond is already false
      }
    }
    cond
  }
}

object TreeCompleteAndLeftAligned extends App {

  val treeCompleteAndLeftAligned = new TreeCompleteAndLeftAligned()

  //t1
  val t1 = new TreeNode(new TreeNode(new TreeNode(null, null), new TreeNode(null, null)), new TreeNode(new TreeNode(null,null), null))

  assert (treeCompleteAndLeftAligned.solve(t1) == true)

 //t2
  val t2 = new TreeNode(new TreeNode(new TreeNode(null, null), new TreeNode(null, null)), new TreeNode( null, new TreeNode(null,null)))
  assert (treeCompleteAndLeftAligned.solve(t2) ==  false)



}

// sent@fb.com
