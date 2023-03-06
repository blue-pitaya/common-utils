package xyz.bluepitaya.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringHelpersSpec extends AnyFlatSpec with Matchers {
  import StringUtils.RichString

  "get1DPositionFrom2DPosition" should "be good" in {
    val text = """1
    |2
    |test
    |yes yes""".stripMargin
    val testResult = text.get1DPositionFrom2DPosition(2, 2)
    val expected = Some(6)

    testResult shouldEqual expected
  }
}
