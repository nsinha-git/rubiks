package nsinha

object Utilities {

  def move(oldPos: (Int, Int, Int), currAxis: Int, from: Int, to: Int, n: Int): Int = {
    /*
    3 -> 2 == 2->-3 == -3 ->-2 == -2 ->3:
    (x,0,0),(x,0,1),(x,0,2) -> (x,0,2), (x,1,2), (x,2,2)
    (x,1,0),(x,1,1),(x,1,2) -> (x,0,1), (x,1,1), (x,2,1)
    (x,2,0),(x,2,1),(x,2,2) -> (x,0,0), (x,1,0), (x,2,0)
    */
    if (Set(from, to, -to).contains(currAxis)) {
      if (from == currAxis) {
        if (to > 0) {
          //-to moves to from
          n -1 - getTupleValAt(oldPos, to)
        } else {
          //to < 0 so abs(to) moves to from
          getTupleValAt(oldPos, to)
        }
      } else {
        //currAxis must be to or -to
        if (to > 0) {
          //from moves to 'to'
          getTupleValAt(oldPos, from)
        } else {
          //-from moves to to
          n -1 - getTupleValAt(oldPos, from)
        }
      }
    } else {
      getTupleValAt(oldPos, currAxis)
    }
  }

  def getTupleValAt(tuple: (Int, Int, Int), i: Int): Int = {
    import math._
    if (abs(i) == 1) {
      tuple._1
    } else if (abs(i) == 2) {
      tuple._2
    } else {
      tuple._3
    }
  }

  def fixOrientation(cube: Cube, fromAxis: Int, toAxis: Int) = {
    if (fromAxis == 1) {
      if (toAxis == 2) {
        cube.orientX = orient(cube.orientX, YOrientation)
        cube.orientY = orient(cube.orientY, MinusXOrientation)
      } else if (toAxis == -2) {
        cube.orientX = orient(cube.orientX, MinusYOrientation)
        cube.orientY = orient(cube.orientY, XOrientation)
      } else if (toAxis == 3) {
        cube.orientX = orient(cube.orientX, ZOrientation)
        cube.orientZ = orient(cube.orientZ, MinusXOrientation)
      } else if (toAxis == -3) {
        cube.orientX = orient(cube.orientX, MinusZOrientation)
        cube.orientZ = orient(cube.orientZ, XOrientation)
      }
    }

    if (fromAxis == 2) {
      if (toAxis == 1) {
        cube.orientY = orient(cube.orientY, XOrientation)
        cube.orientX = orient(cube.orientX, MinusYOrientation)
      } else if (toAxis == -1) {
        cube.orientY = orient(cube.orientY, MinusXOrientation)
        cube.orientX = orient(cube.orientX, YOrientation)
      } else if (toAxis == 3) {
        cube.orientY = orient(cube.orientY, ZOrientation)
        cube.orientZ = orient(cube.orientZ, MinusYOrientation)
      } else if (toAxis == -3) {
        cube.orientY = orient(cube.orientY, MinusZOrientation)
        cube.orientZ = orient(cube.orientZ, YOrientation)
      }
    }

    if (fromAxis == 3) {
      if (toAxis == 2) {
        cube.orientZ = orient(cube.orientZ, YOrientation)
        cube.orientY = orient(cube.orientY, MinusZOrientation)
      } else if (toAxis == -2) {
        cube.orientZ = orient(cube.orientZ, MinusYOrientation)
        cube.orientY = orient(cube.orientY, ZOrientation)
      } else if (toAxis == 1) {
        cube.orientZ = orient(cube.orientZ, XOrientation)
        cube.orientX = orient(cube.orientX, MinusZOrientation)
      } else if (toAxis == -1) {
        cube.orientZ = orient(cube.orientZ, MinusXOrientation)
        cube.orientX = orient(cube.orientX, ZOrientation)
      }
    }
  }

  def orient(orig: Orientation, rot: Orientation): Orientation = {
    (orig, rot) match {
      case (XOrientation, XOrientation) => XOrientation
      case (XOrientation, MinusXOrientation) => XOrientation
      case (XOrientation, YOrientation) => MinusZOrientation
      case (XOrientation, MinusYOrientation) => ZOrientation
      case (XOrientation, ZOrientation) => MinusYOrientation
      case (XOrientation, MinusZOrientation) => YOrientation

      case (MinusXOrientation, XOrientation) => MinusXOrientation
      case (MinusXOrientation, MinusXOrientation) => MinusXOrientation
      case (MinusXOrientation, YOrientation) => ZOrientation
      case (MinusXOrientation, MinusYOrientation) => MinusZOrientation
      case (MinusXOrientation, ZOrientation) => YOrientation
      case (MinusXOrientation, MinusZOrientation) => MinusYOrientation

      case (YOrientation, XOrientation) => MinusZOrientation
      case (YOrientation, MinusXOrientation) => XOrientation
      case (YOrientation, YOrientation) => YOrientation
      case (YOrientation, MinusYOrientation) => YOrientation
      case (YOrientation, ZOrientation) => XOrientation
      case (YOrientation, MinusZOrientation) => MinusXOrientation

      case (MinusYOrientation, XOrientation) => ZOrientation
      case (MinusYOrientation, MinusXOrientation) => MinusZOrientation
      case (MinusYOrientation, YOrientation) => MinusYOrientation
      case (MinusYOrientation, MinusYOrientation) => MinusYOrientation
      case (MinusYOrientation, ZOrientation) => MinusXOrientation
      case (MinusYOrientation, MinusZOrientation) => XOrientation

      case (ZOrientation, XOrientation) => YOrientation
      case (ZOrientation, MinusXOrientation) => MinusYOrientation
      case (ZOrientation, YOrientation) =>  XOrientation
      case (ZOrientation, MinusYOrientation) => MinusXOrientation
      case (ZOrientation, ZOrientation) => ZOrientation
      case (ZOrientation, MinusZOrientation) => ZOrientation

      case (MinusZOrientation, XOrientation) => MinusYOrientation
      case (MinusZOrientation, MinusXOrientation) => YOrientation
      case (MinusZOrientation, YOrientation) => MinusXOrientation
      case (MinusZOrientation, MinusYOrientation) => XOrientation
      case (MinusZOrientation, ZOrientation) => MinusZOrientation
      case (MinusZOrientation, MinusZOrientation) => MinusZOrientation
    }

  }
}
