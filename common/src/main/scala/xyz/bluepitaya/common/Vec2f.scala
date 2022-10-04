package xyz.bluepitaya.common

case class Vec2f(val x: Double, val y: Double) {
  def +(v: Vec2f) = Vec2f(x + v.x, y + v.y)
  def -(v: Vec2f) = Vec2f(x - v.x, y - v.y)
  def *(n: Double) = Vec2f(x * n, y * n)
  def /(n: Double) = Vec2f(x / n, y / n)

  lazy val magnitude = Math.sqrt(x * x + y * y)
  lazy val normalize = this / magnitude
  lazy val floor = Vec2f(Math.floor(x), Math.floor(y))

  def changeIfZero(v: => Double) =
    Vec2f(x = if (x == 0) v else x, y = if (y == 0) v else y)

  def round(precisiton: Int) = {
    def r(v: Double) = BigDecimal(v)
      .setScale(precisiton, BigDecimal.RoundingMode.HALF_UP)
      .toDouble
    Vec2f(r(x), r(y))
  }
}

object Vec2f {
  def zero = Vec2f(0.0, 0.0)
}
