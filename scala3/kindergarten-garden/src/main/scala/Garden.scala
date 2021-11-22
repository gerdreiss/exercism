enum Plant(ch: Char):
  case Clover extends Plant('C')
  case Grass extends Plant('G')
  case Radishes extends Plant('R')
  case Violets extends Plant('V')

object Plant:
  def fromChar(ch: Char): Option[Plant] =
    ch match
      case 'C' => Some(Clover)
      case 'G' => Some(Grass)
      case 'R' => Some(Radishes)
      case 'V' => Some(Violets)
      case _   => None

case class Child(name: String, plants: List[Plant])

case class Garden(children: List[Child]):
  def plants(name: String): List[Plant] =
    children.filter(_.name == name).flatMap(_.plants)

object Garden:
  val children =
    List(
      "Alice",
      "Bob",
      "Charlie",
      "David",
      "Eve",
      "Fred",
      "Ginny",
      "Harriet",
      "Ileana",
      "Joseph",
      "Kincaid",
      "Larry"
    )

  def defaultGarden(plants: String): Garden =
    Garden(children.zip(groupedBy4(plants)).map(Child.apply.tupled))

  def groupedBy4(plants: String): List[List[Plant]] =
    plants.zipLines
      .sliding(2, 2)
      .foldLeft(List.empty[Plant]) {
        case (acc, (p00, p10) :: (p01, p11) :: _) =>
          acc ++ List(p00, p01, p10, p11).flatMap(Plant.fromChar)
        case (acc, _) => acc
      }
      .grouped(4)
      .toList

  extension (lines: String)
    def zipLines: List[(Char, Char)] =
      val split = lines.split('\n')
      split(0).zip(split(1)).toList
