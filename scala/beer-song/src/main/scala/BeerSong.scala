object BeerSong {

  def recite(bottles: Int, verses: Int): String = {
    if (verses == 0) ""
    else s"${line1(bottles)}${line2(bottles)}${newline(verses)}${recite(bottles - 1, verses - 1)}"
  }

  private def line1(bottles: Int): String =
    s"${bottleCount(bottles)} of beer on the wall, ${bottleCount(bottles).toLowerCase} of beer.\n"

  private def bottleCount(bottles: Int): String =
    bottles match {
      case 0 => "No more bottles"
      case 1 => "1 bottle"
      case _ => s"$bottles bottles"
    }

  private def line2(bottles: Int): String =
    s"${action(bottles)}, ${actionResult(bottles)} of beer on the wall.\n"

  private def action(bottles: Int): String =
    bottles match {
      case 0 => "Go to the store and buy some more"
      case 1 => "Take it down and pass it around"
      case _ => "Take one down and pass it around"
    }

  private def actionResult(bottles: Int): String =
    bottles match {
      case 0 => "99 bottles"
      case 1 => "no more bottles"
      case 2 => "1 bottle"
      case _ => s"${bottles - 1} bottles"
    }

  private def newline(verses: Int): String =
    if (verses <= 1) ""
    else "\n"
}
