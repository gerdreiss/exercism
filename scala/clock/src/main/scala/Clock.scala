case class HM(hours: Int, minutes: Int) {
  def +(that: HM): HM = {
    val sumMinutes = this.minutes + that.minutes
    val sumHours   = this.hours + that.hours + sumMinutes / 60
    Clock.apply(sumHours % 24, sumMinutes % 60)
  }
  def -(that: HM): HM =
    this.+(that.copy(hours = -that.hours, minutes = -that.minutes))
}

object Clock {
  def apply(hours: Int, minutes: Int): HM =
    if (hours < 0)
      apply(24 + hours, minutes)
    else if (minutes < 0)
      apply(hours - 1, 60 + minutes)
    else
      HM((hours + minutes / 60) % 24, minutes % 60)

  def apply(minutes: Int): HM =
    apply(minutes / 60, minutes % 60)

}
