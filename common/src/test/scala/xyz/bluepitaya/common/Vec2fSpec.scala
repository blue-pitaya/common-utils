package xyz.bluepitaya.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Vec2fSpec extends AnyFlatSpec with Matchers {
  "changeIfZero" should "handle unpure functions correctly" in {
    val v = Vec2f.zero

    var i: Double = 0
    def getI = {
      i += 1
      i
    }

    v.changeIfZero(getI) shouldEqual Vec2f(1.0, 2.0)
  }
}
