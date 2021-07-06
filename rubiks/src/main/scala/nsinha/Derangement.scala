package nsinha

case class Derangement(xOrent: Orientation, yOrient: Orientation, zOrient: Orientation, xPos: (Int, Int), yPos: (Int, Int), zPos: (Int, Int))


object Derangement {
  def getDerangementCube(c: Cube): Derangement = {
    Derangement(c.orientX, c.orientY, c.orientZ, (c.origX, c.currX), (c.origY, c.currY), (c.origZ, c.currZ))
  }
  //(x-Y): (1,-1, 2,-2, 3,-3) (2, -2, -1, 1, 3, -3): -y to X y to -x
  //       (2,-2,-1, 1, 3,-3)
}
