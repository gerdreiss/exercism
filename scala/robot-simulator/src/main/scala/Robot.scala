sealed trait Bearing {
  def turnLeft: Bearing
  def turnRight: Bearing
}
object Bearing       {
  case object North extends Bearing {
    override val turnLeft: Bearing  = Bearing.West
    override val turnRight: Bearing = Bearing.East
  }
  case object South extends Bearing {
    override val turnLeft: Bearing  = Bearing.East
    override val turnRight: Bearing = Bearing.West
  }
  case object East  extends Bearing {
    override val turnLeft: Bearing  = Bearing.North
    override val turnRight: Bearing = Bearing.South
  }
  case object West  extends Bearing {
    override val turnLeft: Bearing  = Bearing.South
    override val turnRight: Bearing = Bearing.North
  }
}

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {
  def turnLeft: Robot                = copy(bearing = bearing.turnLeft)
  def turnRight: Robot               = copy(bearing = bearing.turnRight)
  def advance: Robot                 = this match {
    case Robot(Bearing.North, (x, y)) => copy(coordinates = (x, y + 1))
    case Robot(Bearing.South, (x, y)) => copy(coordinates = (x, y - 1))
    case Robot(Bearing.East, (x, y))  => copy(coordinates = (x + 1, y))
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
