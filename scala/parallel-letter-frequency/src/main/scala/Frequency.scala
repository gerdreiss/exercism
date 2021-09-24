object Frequency {

  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] = {
    if (numWorkers == 0 || texts.isEmpty) Map.empty
    else
      texts
        .grouped(if (texts.length < numWorkers) 1 else texts.length / numWorkers)
        .toParArray
        .map(_.mkString.toLowerCase.filter(_.isLetter).groupBy(identity).mapValues(_.length))
        .reduce((x, y) => (x.keySet ++ y.keySet).map(ch => ch -> (x.getOrElse(ch, 0) + y.getOrElse(ch, 0))).toMap)
  }

}
