import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

class Robot(val letters: Int = 2, val digits: Int = 3) {

  private var _name = RobotNames.generate(letters, digits)

  def name: String = _name

  def reset(): Unit = {
    _name = RobotNames.generate(letters, digits)
  }
}

object RobotNames {
  private val nameRegex = "([A-Z]{2}[0-9]{3})".r
  private val cache     = mutable.ListBuffer.empty[String]

  @tailrec
  def generate(letters: Int = 2, digits: Int = 3): String   =
    randomName(letters, digits) match {
      case nameRegex(s) if !cache.contains(s) => cache.append(s); s
      case _                                  => generate(letters, digits)
    }
  private def randomName(letters: Int = 2, digits: Int = 3) =
    (0 until letters).map(_ => Random.nextPrintableChar.toUpper).mkString +
      (0 until digits).map(_ => Random.nextInt(10)).mkString

}
