import scala.annotation.tailrec
import scala.util.Random

class Robot(val letters: Int = 2, val digits: Int = 3) {
  private val nameRegex = "([A-Z]{2}[0-9]{3})".r
  private var cache     = List(generate)

  @tailrec
  private def generate: String = {
    s"$randomChar$randomChar$randomInt$randomInt$randomInt" match {
      case nameRegex(s) => s
      case _            => generate
    }
  }
  private def randomChar = Random.nextPrintableChar().toUpper
  private def randomInt  = Random.nextInt(10)

  def name: String = cache.head

  def reset(): Unit = {
    val newName = generate
    if (!cache.contains(newName)) cache = newName :: cache
    else reset()
  }

}
