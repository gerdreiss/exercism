sealed trait Bearing {
  def turnLeft: Bearing
  def turnRight: Bearing
}
object Bearing       {
  case object North extends Bearing {
    override def turnLeft: Bearing  = Bearing.West
    override def turnRight: Bearing = Bearing.East
  }
  case object East  extends Bearing {
    override def turnLeft: Bearing  = Bearing.North
    override def turnRight: Bearing = Bearing.South
  }
  case object South extends Bearing {
    override def turnLeft: Bearing  = Bearing.East
    override def turnRight: Bearing = Bearing.West
  }
  case object West  extends Bearing {
    override def turnLeft: Bearing  = Bearing.South
    override def turnRight: Bearing = Bearing.North
  }
}

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {
  def turnRight: Robot               = copy(bearing = bearing.turnRight)
  def turnLeft: Robot                = copy(bearing = bearing.turnLeft)
  def advance: Robot                 = this match {
    case Robot(Bearing.North, (x, y)) => copy(coordinates = (x, y + 1))
    case Robot(Bearing.East, (x, y))  => copy(coordinates = (x + 1, y))
    case Robot(Bearing.South, (x, y)) => copy(coordinates = (x, y - 1))
    case Robot(Bearing.West, (x, y))  => copy(coordinates = (x - 1, y))
  }
  def simulate(steps: String): Robot = steps.foldLeft(this) { (r, ch) =>
    ch match {
      case 'A' => r.advance
      case 'L' => r.turnLeft
      case 'R' => r.turnRight
    }
  }
}
