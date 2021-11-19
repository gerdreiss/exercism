import scala.util.matching.Regex

object PigLatin {

  val beginsWithVowelSound: Regex                 = "(xr|yt|[aeiou]+).*".r
  val beginsWithConsonantSound: Regex             = "([^aeiou]+).*".r
  val beginsWithConsonantSoundFollowedByQU: Regex = "([^aeiouq]*qu).*".r
  val beginsWithConsonantSoundFollowedByY: Regex  = "([^aeiouy]+)y.*".r

  def movePrefix(word: String, prefix: String): String =
    word.drop(prefix.length) + prefix + "ay"

  def translate(phrase: String): String =
    phrase
      .split("\\s")
      .map {
        case word @ beginsWithVowelSound(_)                      => word + "ay"
        case word @ beginsWithConsonantSoundFollowedByQU(prefix) => movePrefix(word, prefix)
        case word @ beginsWithConsonantSoundFollowedByY(prefix)  => movePrefix(word, prefix)
        case word @ beginsWithConsonantSound(prefix)             => movePrefix(word, prefix)
        case word                                                => word
      }
      .mkString(" ")

}
