case class Triangle(side1: Double, side2: Double, side3: Double) {

  private val sides = Set(side1, side2, side3)

  private val legal: Boolean =
    sides.forall(_ > 0.0) &&
      (side1 + side2) >= side3 &&
      (side1 + side3) >= side2 &&
      (side2 + side3) >= side1

  // An equilateral triangle has all three sides the same length
  val equilateral: Boolean = legal && sides.size == 1
  // An isosceles triangle has at least two sides the same length
  val isosceles: Boolean   = legal && sides.size <= 2
  // A scalene triangle has all sides of different lengths.
  val scalene: Boolean     = legal && sides.size == 3

}
