sealed trait Plant
object Plant {
  case object Clover   extends Plant
  case object Grass    extends Plant
  case object Radishes extends Plant
  case object Violets  extends Plant

  def fromCharacter(c: Char): Option[Plant] =
    c match {
      case 'C' => Some(Clover)
      case 'G' => Some(Grass)
      case 'R' => Some(Radishes)
      case 'V' => Some(Violets)
      case _   => None
    }
}

case class Child(name: String, plants: List[Plant])
case class Garden(children: List[Child]) {
  def plants(name: String): List[Plant] =
    children.filter(_.name == name).flatMap(_.plants)
}

object Garden {
  val children =
    List("Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry")

  def defaultGarden(plants: String): Garden =
    Garden(children.zip(groupedBy4(plants)).map(Child.tupled))

  def groupedBy4(plants: String): List[List[Plant]] =
    (plants.takeWhile(_ != '\n') zip plants.dropWhile(_ != '\n').drop(1)).toList
      .sliding(2, 2)
      .foldLeft(List.empty[Plant]) { case (acc, (p00, p10) :: (p01, p11) :: _) =>
        acc ++ List(p00, p01, p10, p11).flatMap(Plant.fromCharacter)
      }
      .grouped(4)
      .toList
}
