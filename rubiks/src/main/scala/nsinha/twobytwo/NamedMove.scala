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
    val pos = rubik.frontPos
    rubik.makeMove(YAxis, XAxis, pos)
  }
}

object DoFt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.frontPos
    rubik.makeMove(XAxis, YAxis, pos)
  }
}

object DoMF extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(YAxis, XAxis, pos)
  }
}

object DoMFt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(XAxis, YAxis, pos)
  }
}
object DoB extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.backPos
    rubik.makeMove(YAxis, XAxis, pos)
  }
}
object DoBt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.backPos
    rubik.makeMove(XAxis, YAxis, pos)
  }
}
object DoMB extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.backPos
    rubik.makeMove(YAxis, XAxis, pos)
  }
}
object DoMBt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.backPos
    rubik.makeMove(XAxis, YAxis, pos)
  }
}
object DoL extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.leftPos
    rubik.makeMove(YAxis, ZAxis, pos)
  }
}

object DoLt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.leftPos
    rubik.makeMove(ZAxis, YAxis, pos)
  }
}

object DoML extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(YAxis, ZAxis, pos)
  }
}

object DoMLt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(ZAxis, YAxis, pos)
  }
}
object DoR extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.rightPos
    rubik.makeMove(ZAxis, YAxis, pos)
  }
}
object DoRt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.rightPos
    rubik.makeMove(YAxis, ZAxis, pos)
  }
}
object DoMR extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(ZAxis, YAxis, pos)
  }
}
object DoMRt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(YAxis, ZAxis, pos)
  }
}
object DoT extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.topPos
    rubik.makeMove(XAxis, ZAxis, pos)
  }
}

object DoTt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.topPos
    rubik.makeMove(ZAxis, XAxis, pos)
  }
}

object DoMT extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(XAxis, ZAxis, pos)
  }
}

object DoMTt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(ZAxis, XAxis, pos)
  }
}

object DoD extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.downPos
    rubik.makeMove(ZAxis, XAxis, pos)
  }
}

object DoDt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.downPos
    rubik.makeMove(XAxis, ZAxis, pos)
  }
}

object DoMD extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(ZAxis, XAxis, pos)
  }
}

object DoMDt extends NamedMove {
  override def doMove(rubik: RubiksCube): Unit = {
    val pos = rubik.middlePos
    rubik.makeMove(XAxis, ZAxis, pos)
  }
}










