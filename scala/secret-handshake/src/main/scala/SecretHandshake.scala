object SecretHandshake {
  val actions = Map(
    0 -> ((acc: List[String]) => acc :+ "wink"),
    1 -> ((acc: List[String]) => acc :+ "double blink"),
    2 -> ((acc: List[String]) => acc :+ "close your eyes"),
    3 -> ((acc: List[String]) => acc :+ "jump"),
    4 -> ((acc: List[String]) => acc.reverse)
  )

  def commands(input: Int): List[String] =
    toBase2(input / 2, input % 2).zipWithIndex
      .foldLeft(List.empty[String]) { case (acc, (c, i)) =>
        (c, i) match {
          case ('1', i) => actions.get(i).map(_.apply(acc)).getOrElse(acc)
          case _        => acc
        }
      }

  private def toBase2(in: Int, rem: Int): String =
    if (in == 0) rem.toString else rem.toString + toBase2(in / 2, in % 2)

}
