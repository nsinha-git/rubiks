package nsinha.twobytwo

import nsinha.{RubiksCube, XAxis, YAxis, ZAxis}

sealed trait NamedMove {
  def doMove(rubiksCube: RubiksCube): Unit

}

object NamedMove {
  def opposite(n: NamedMove): NamedMove = {
    n match {
      case DoF => DoFt
      case DoFt => DoF
      case DoB => DoBt
      case DoBt => DoB
      case DoL => DoLt
      case DoLt => DoL
      case DoR => DoRt
      case DoRt => DoR
      case DoT => DoTt
      case DoTt => DoT
      case DoD => DoDt
      case DoDt => DoD
      case DoFRFt => DoFRFtt
      case DoFRFtt => DoFRFt
    }
  }
}

object DoFRFt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit =  {
    DoF.doMove(rubik)
    DoR.doMove(rubik)
    DoFt.doMove(rubik)
  }
}


object DoLFLt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit =  {
    DoL.doMove(rubik)
    DoF.doMove(rubik)
    DoLt.doMove(rubik)
  }
}

object DoFRFtt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    DoFt.doMove(rubik)
    DoRt.doMove(rubik)
    DoF.doMove(rubik)
  }
}

object DoF extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (1, 1, 1)
    rubik.makeMove(YAxis, XAxis, pos)
  }
}

object DoFt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (1, 1, 1)
    rubik.makeMove(XAxis, YAxis, pos)
  }
}
object DoB extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (0,0,0)
    rubik.makeMove(YAxis, XAxis, pos)
  }
}
object DoBt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (0,0,0)
    rubik.makeMove(XAxis, YAxis, pos)
  }
}
object DoL extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (0,0,0)
    rubik.makeMove(YAxis, ZAxis, pos)
  }
}

object DoLt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (0,0,0)
    rubik.makeMove(ZAxis, YAxis, pos)
  }
}
object DoR extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (1,1,1)
    rubik.makeMove(ZAxis, YAxis, pos)
  }
}
object DoRt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (1,1,1)
    rubik.makeMove(YAxis, ZAxis, pos)
  }
}
object DoT extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (1,1,1)
    rubik.makeMove(XAxis, ZAxis, pos)
  }
}

object DoTt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (1,1,1)
    rubik.makeMove(ZAxis, XAxis, pos)
  }
}

object DoD extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (0,0,0)
    rubik.makeMove(ZAxis, XAxis, pos)
  }
}

object DoDt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = (0,0,0)
    rubik.makeMove(XAxis, ZAxis, pos)
  }
}





