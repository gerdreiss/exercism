object Raindrops {
  def convert(n: Int): String = {
    val result = Map(3 -> "Pling", 5 -> "Plang", 7 -> "Plong")
      .foldLeft("") { case (acc, (factor, s)) =>
        if (n % factor == 0) acc + s else acc
      }
    if (result.isEmpty) n.toString else result
  }
}
