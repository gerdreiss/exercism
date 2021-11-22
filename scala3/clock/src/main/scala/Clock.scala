// case class Clock private(hours: Int, minutes: Int, seconds: Int):
//   def +(that: Clock): Clock =
//     val sumMinutes = this.minutes + that.minutes + seconds / 60
//     val sumHours   = this.hours + that.hours + sumMinutes / 60
//     Clock.fromHM(sumHours % 24, sumMinutes % 60)
//   def -(that: Clock): Clock =
//     this.+(that.copy(hours = -that.hours, minutes = -that.minutes))

case class Clock private (hours: Int, minutes: Int, seconds: Int)

object Clock extends Numeric[Clock]:
  def apply(hours: Int, minutes: Int): Clock =
    fromHM(hours, minutes)

  def apply(minutes: Int): Clock =
    fromM(minutes)

  private def fromHM(hours: Int, minutes: Int): Clock =
    if hours < 0 then apply(24 + hours, minutes)
    else if minutes < 0 then apply(hours - 1, 60 + minutes)
    else Clock((hours + minutes / 60) % 24, minutes % 60, 0)

  private def fromM(minutes: Int): Clock =
    fromHM(minutes / 60, minutes % 60)

  override def plus(x: Clock, y: Clock): Clock =
    val sumMinutes = x.minutes + y.minutes
    val sumHours   = x.hours + y.hours + sumMinutes / 60
    fromHM(sumHours % 24, sumMinutes % 60)

  override def minus(x: Clock, y: Clock): Clock =
    plus(x, negate(y))

  override def times(x: Clock, y: Clock): Clock = ???

  override def negate(x: Clock): Clock =
    x.copy(hours = -x.hours, minutes = -x.minutes)

  override def fromInt(x: Int): Clock =
    fromM(x)

  override def toInt(x: Clock): Int =
    x.hours * 60 + x.minutes

  override def toLong(x: Clock): Long =
    toInt(x).toLong

  override def toFloat(x: Clock): Float =
    toInt(x).toFloat

  override def toDouble(x: Clock): Double =
    toInt(x).toDouble

  override def compare(x: Clock, y: Clock): Int =
    toInt(x).compare(toInt(y))

  override def parseString(str: String): Option[Clock] = ???

extension (lhs: Clock)
  def +(rhs: Clock)    = Clock.plus(lhs, rhs)
  def -(rhs: Clock)    = Clock.minus(lhs, rhs)
  def *(rhs: Clock)    = Clock.times(lhs, rhs)
  def unary_-          = Clock.negate(lhs)
  def abs: Clock       = Clock.abs(lhs)
  def sign: Clock      = Clock.sign(lhs)
  def toInt: Int       = Clock.toInt(lhs)
  def toLong: Long     = Clock.toLong(lhs)
  def toFloat: Float   = Clock.toFloat(lhs)
  def toDouble: Double = Clock.toDouble(lhs)
