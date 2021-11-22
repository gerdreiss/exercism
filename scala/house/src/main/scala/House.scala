object House {

  val clauses = List(
    "the malt that lay in ",
    "the rat that ate ",
    "the cat that killed ",
    "the dog that worried ",
    "the cow with the crumpled horn that tossed ",
    "the maiden all forlorn that milked ",
    "the man all tattered and torn that kissed ",
    "the priest all shaven and shorn that married ",
    "the rooster that crowed in the morn that woke ",
    "the farmer sowing his corn that kept ",
    "the horse and the hound and the horn that belonged to "
  )

  def recite(x: Int, y: Int): String =
    (x to y).map(withClauses).mkString + "\n"

  def withClauses(x: Int): String =
    s"This is ${insertClauses(x - 1)}the house that Jack built.\n"

  def insertClauses(x: Int): String =
    if (x <= 0) ""
    else clauses(x - 1) + insertClauses(x - 1)

}
