import scala.util.matching.Regex

object PigLatin:

  val BeginsWithVowelSound: Regex                 = "(xr|yt|[aeiou]+)(.*)".r
  val BeginsWithConsonantSound: Regex             = "([^aeiou]+)(.*)".r
  val BeginsWithConsonantSoundFollowedByQu: Regex = "([^aeiouq]*qu)(.*)".r
  val BeginsWithConsonantSoundFollowedByY: Regex  = "([^aeiouy]+)(y.*)".r

  extension (s: String) def words: Array[String] = s.split("\\s")

  def translate(phrase: String): String =
    phrase.words
      .map {
        case BeginsWithVowelSound(prefix, rest)                 => prefix + rest + "ay"
        case BeginsWithConsonantSoundFollowedByQu(prefix, rest) => rest + prefix + "ay"
        case BeginsWithConsonantSoundFollowedByY(prefix, rest)  => rest + prefix + "ay"
        case BeginsWithConsonantSound(prefix, rest)             => rest + prefix + "ay"
        case word                                               => word
      }
      .mkString(" ")
