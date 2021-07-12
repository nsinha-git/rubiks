package nsinha

sealed trait Axis
object XAxis extends Axis {
  override def toString: String = "XAxis"
}
object YAxis extends Axis {
  override def toString: String = "YAxis"
}
object ZAxis extends Axis {
  override def toString: String = "ZAxis"
}
object NegXAxis extends Axis {
  override def toString: String = "NegXAxis"
}
object NegYAxis extends Axis {
  override def toString: String = "NegYAxis"
}
object NegZAxis extends Axis {
  override def toString: String = "NegZAxis"
}

object Axis {
  type T = (Int, Int, Int)
  def convertAxisToCordinate(axis: Axis): T = {
    axis match {
      case XAxis => (1,0,0)
      case YAxis => (0,1,0)
      case ZAxis => (0,0,1)
      case NegXAxis => (-1,0,0)
      case NegYAxis => (0,-1,0)
      case NegZAxis => (0,0,-1)
    }
  }

  def or (x: T, y: T): T = {
    (x._1 | y._1,  x._2 | y._2, x._3 | y._3)
  }

  /*
  axes are based on start count of 1 2 and 3 so that we can differentiate between +-
   */
  def findUniqueZero(x : T): Int = {
    if (x._1 == 0 && x._2 !=0 && x._3 != 0) {
      1
    } else if (x._2 == 0 && x._1 !=0 && x._3 != 0) {
      2
    } else if (x._3 == 0 && x._2 !=0 && x._1 != 0){
      3
    } else {
      assert(false)
      0
    }
  }

  //axes are based on start count of 1 2 and 3 so that we can differentiate between +-
  def findUniqueOne(x : T): Int = {
    if (x._1 != 0 && x._2 ==0 && x._3 == 0) {
      1
    } else if (x._2 != 0 && x._1 ==0 && x._3 == 0) {
      2
    } else if (x._3 != 0 && x._2 ==0 && x._1 == 0){
      3
    } else {
      assert(false)
      0
    }
  }

  def sign(x: T): Int = {
    x._1 + x._2 + x._3
  }

  def getCoordOfPoint(x: T, i: Int): Int = {
    (i - 1) match {
      case 0 => x._1
      case 1 => x._2
      case 2 => x._3
    }
  }
}

