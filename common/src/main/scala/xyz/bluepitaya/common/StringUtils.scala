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

    def stringDropUntil(pred: String => Boolean): String =
      if (str.isEmpty()) ""
      else if (pred(str)) str
      else str.stringDropUntil(pred)
  }
}
