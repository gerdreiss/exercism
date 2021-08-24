import java.time.LocalDate
import java.time.LocalDateTime

object Gigasecond {
  def add(startDate: LocalDate): LocalDateTime =
    startDate.atStartOfDay().plusSeconds(1000000000)

  def add(startDateTime: LocalDateTime): LocalDateTime =
    startDateTime.plusSeconds(1000000000)
}
