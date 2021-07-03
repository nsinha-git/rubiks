package nsinha

case class Cube(origX: Int, origY: Int, origZ: Int, var currX: Int, var currY: Int, var currZ: Int,
                var orientX: Orientation = XOrientation, var orientY: Orientation = YOrientation, var orientZ: Orientation = ZOrientation) {
}
