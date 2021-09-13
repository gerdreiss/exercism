object Etl {
  def transform(m: Map[Int, Seq[String]]): Map[String, Int] =
    m.flatMap { case (i, ss) =>
      ss.map(_.toLowerCase -> i)
    }
}
