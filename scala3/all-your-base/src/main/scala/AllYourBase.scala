object AllYourBase:
  def getInputValue(inputBase: Int, input: List[Int]): Option[Int] = Some(
    input
      .zip(input.indices.reverse)
      .map { (n, idx) =>
        n * math.pow(inputBase, idx).toInt
      }
      .sum
  )

  def getOutputDigits(outputBase: Int)(inputValue: Int): List[Int] =
    if inputValue == 0 then Nil
    else inputValue % outputBase :: getOutputDigits(outputBase)(inputValue / outputBase)

  def rebase(inputBase: Int, input: List[Int], outputBase: Int): Option[List[Int]] =
    if inputBase < 2 || outputBase < 2 || input.exists(_ < 0) || input.exists(_ >= inputBase) then None
    else if input.isEmpty || input.sum == 0 then Some(List(0))
    else getInputValue(inputBase, input).map(getOutputDigits(outputBase)).map(_.reverse)
