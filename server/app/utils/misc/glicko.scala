package utils.misc

// https://en.wikipedia.org/wiki/Glicko_rating_system

case class GlickoData(
  rating: Double = Glicko.rating0,
  rd: Double = Glicko.rd0,
  lastrated: Double = 0.0
)

object Glicko {

  ///////////////////////////////////////////
  // Glicko constants

  var rating0 = 1200.0
  val rd0 = 350.0

  def typrd = 50.0

  def timeconst = 1000.0

  val RATING_DIFFERENCE_DIVISOR = 400.0

  val MIN_RATING = 100.0
  val MAX_RATING = 3500.0

  ///////////////////////////////////////////
  // Math utils

  val Pi = scala.math.Pi
  val q: Double = scala.math.log(10.0) / RATING_DIFFERENCE_DIVISOR

  def sqrt(x: Double) = scala.math.sqrt(x)
  def sq(x: Double) = x * x
  def pow10(x: Double) = scala.math.pow(10.0, x)
  def min(x1: Double, x2: Double) = scala.math.min(x1, x2)

  ///////////////////////////////////////////
  // Glick sub calculations

  def g(rdi: Double): Double = 1.0 / sqrt(1.0 + 3.0 * sq(q * rdi / Pi))

  def E(r: Double, ri: Double, rdi: Double): Double = 1.0 / (1.0 + pow10(g(rdi) * (r - ri) / -RATING_DIFFERENCE_DIVISOR))

  def d2(r: Double, ri: Double, rdi: Double): Double = 1.0 / (sq(q) * sq(g(rdi)) * E(r, ri, rdi) * (1 - E(r, ri, rdi)))

  def r(r: Double, rd: Double, ri: Double, rdi: Double, si: Double): Double = {
    var newr = r + q / ((1 / sq(rd) + (1 / d2(r, ri, rdi)))) * (si - E(r, ri, rdi))
    if (newr < MIN_RATING) newr = MIN_RATING
    if (newr > MAX_RATING) newr = MAX_RATING
    newr
  }

  def c2 = (sq(rd0) - sq(typrd)) / shared.SharedTimeUtils.MonthMS

  def getrdt(rd: Double, t: Double): Double = min(sqrt(sq(rd) + c2 * t), rd0)

  def rd(r: Double, rd: Double, ri: Double, rdi: Double): Double =
    sqrt(1 / ((1 / sq(rd)) + (1 / d2(r, ri, rdi))))

  ///////////////////////////////////////////
  // Glick calculation

  def calc(g: GlickoData, gi: GlickoData, si: Double, verbose: Boolean = false): GlickoData = {
    val now = System.currentTimeMillis().toDouble
    val rdt = getrdt(g.rd, now - g.lastrated)

    if (verbose) {
      println("***********************************")
      println("Glicko calculation")
      println("***********************************")
      println("Player " + g)
      println("Opponent " + gi)
      println("***********************************")
      println("Result " + si)
      println("Expected result " + E(g.rating, gi.rating, gi.rd))
      println("***********************************")
    }

    val result = GlickoData(
      rating = r(g.rating, rdt, gi.rating, gi.rd, si),
      rd = rd(g.rating, rdt, gi.rating, gi.rd),
      now
    )

    if (verbose) {
      println("New rating " + result)
      println("***********************************")
    }

    result
  }

  ///////////////////////////////////////////

}