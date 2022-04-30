package org.nsinha
package trees

class HeapSort(sz: Int, maxOrMin: Int) {
  private val data: Array[Integer] = new Array[Integer](sz)
  private val heapFn = if (maxOrMin > 0) {
    math.max
  } else {
    math.min
  }
  private var nextIdx = 0

  private def getKidIndex(n: Int) = {
    (2 * n + 1, 2 * n + 2)
  }

  private def getParentIdx(n: Int): Int = {
    (n-1) >> 1
  }


  def insertNext(v: Int): Boolean = {
    if (nextIdx >= sz) {
      false
    } else {
      data(nextIdx) = v
      nextIdx += 1
      true
    }
  }

  def heapify()  = {
    val curSize = nextIdx
    //heapyfy from 0 to curSize(exclusive)
    while (heapifyOnNode(0, true)) {
    }
  }

  private def heapifyOnNode(idx: Int, fullHeapyfy: Boolean = false): Boolean = {
    // returns true if structure under it changes else true
    if(idx >= nextIdx){
      return false
    }
    val r = data(idx)
    val (kids1, kids2) = getKidIndex(idx)
    if (kids1 >= nextIdx-1) {
      return false
    }
    val k1 = data(kids1)
    val k2: Int = if (kids2 >= nextIdx -1) {
      data(sz-1) + 1
    } else {
      data(kids2)
    }
    var ch = true
    var changedIdx = -1

    if (heapFn(heapFn(k1, k2), r) != r) {
      val k11  = heapFn(k1, k2)
      if (k11 == k1) {
        data(kids1) = r
        data(idx) = k1
        changedIdx = 1
      } else {
        data(kids2) = r
        data(idx) = k2
        changedIdx = 2
      }
      ch = true
    } else {
      ch = false
    }
    if (fullHeapyfy) {
      ch |= heapifyOnNode(kids1, fullHeapyfy)
      ch |= heapifyOnNode(kids2, fullHeapyfy)
      ch
    } else {
      if (changedIdx == 1) {
        ch |= heapifyOnNode(kids1, fullHeapyfy)
        ch
      } else {
        ch |= heapifyOnNode(kids2, fullHeapyfy)
        ch
      }
    }
  }

  def removeTop(): Int = {
    val v = data(0)
    val replacevalue = data(nextIdx-1)
    data(0) = replacevalue
    heapifyOnNode(0)
    nextIdx -=1
    v
  }
}

object HeapSort {

  def main(args: Array[String]): Unit = {
    val heapSort = new HeapSort(127, 1)
    for (i <- Range(0, 127)) {
      heapSort.insertNext(i)
    }
    heapSort.heapify()
    for (i <- Range(0, 127)) {
      heapSort.insertNext(i+200)
    }
    for (i <- Range(0, 127)) {
      println(heapSort.removeTop())
    }

  }
}
