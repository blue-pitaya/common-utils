package xyz.bluepitaya.common

object StringUtils {
  implicit class RichString(str: String) {
    def countChar(ch: Char): Int = {
      def f(text: String, n: Int): Int = text.headOption match {
        case Some(c) if c == ch => f(text.tail, n + 1)
        case _                  => n
      }
      f(str, 0)
    }

    /** note: will leave empty range on start if first x is 0 */
    def extractRanges(orderedRangeFromXs: List[Int]): List[Range] = {
      val beforeStartPatternRange = orderedRangeFromXs.headOption match {
        case None        => (0, str.size)
        case Some(value) => (0, value)
      }

      val rangeUntils = orderedRangeFromXs.drop(1) :+ str.size
      val ranges = beforeStartPatternRange +:
        orderedRangeFromXs.zip(rangeUntils)

      ranges.map { case (start, end) => Range(start, end) }
    }

    def extractRangesEnding(orderedRangesEndInc: List[Int]): List[Range] = {
      val rangeFroms = 0 +: orderedRangesEndInc.map(_ + 1)
      val rangeUntils = orderedRangesEndInc.map(_ + 1) :+ str.size

      rangeFroms
        .zip(rangeUntils)
        .map { case (from, until) => Range(from, until) }
    }

    def extractLineRanges: List[Range] = {
      val sep = """\n""".r
      val fromPoses = sep.findAllMatchIn(str).map(x => x.start).toList

      str.extractRangesEnding(fromPoses)
    }

    def stringDropUntil(pred: String => Boolean): String =
      if (str.isEmpty()) ""
      else if (pred(str)) str
      else str.stringDropUntil(pred)

    def get1DPositionFrom2DPosition(row: Int, col: Int): Option[Int] = {
      val ranges = str.extractLineRanges

      for {
        matchingLineRange <- ranges.lift(row)
      } yield (matchingLineRange.from + col)
    }
  }
}
