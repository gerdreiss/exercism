object Frequency {

  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] = {
    if (texts.isEmpty) Map.empty
    else
      texts
        .grouped(numWorkers)
        .toParArray
        .map(_.mkString.toLowerCase.filter(_.isLetter).groupBy(identity).mapValues(_.length))
        .reduce((x, y) => (x.keySet ++ y.keySet).map(ch => ch -> (x.getOrElse(ch, 0) + y.getOrElse(ch, 0))).toMap)
  }

}
