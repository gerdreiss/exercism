object SecretHandshake {
  def commands(input: Int): List[String] =
    toBase2(input / 2, input % 2)
      .zipWithIndex
      .foldLeft(List.empty[String]) { case (acc, (c, i)) =>
        (c, i) match {
          case ('1', 0) => acc :+ "wink"
          case ('1', 1) => acc :+ "double blink"
          case ('1', 2) => acc :+ "close your eyes"
          case ('1', 3) => acc :+ "jump"
          case ('1', 4) => acc.reverse
          case _        => acc
        }
      }

  private def toBase2(in: Int, rem: Int): String =
    if (in == 0) rem.toString else rem.toString + toBase2(in / 2, in % 2)

}
