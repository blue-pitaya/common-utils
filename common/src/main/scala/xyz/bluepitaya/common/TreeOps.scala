package xyz.bluepitaya.common

trait TreeNode[A] {
  def value(x: A): String
  def childen(x: A): Seq[A]
}

object TreeOps {
  def preOrderPrint[A](node: A, indent: String = "\t")(implicit
      tr: TreeNode[A]
  ): Unit = {
    def f(node: A, depth: Int = 0): Unit = {
      println((indent * depth) + tr.value(node))
      tr.childen(node).foreach(n => f(n, depth + 1))
    }

    f(node)
  }

  // TODO: benchmark k and f (im curious which is faster)
  def preOrderToString[A](node: A, indent: String = "\t")(implicit
      tr: TreeNode[A]
  ): String = {
    // def k(node: A, depth: Int = 0): Seq[String] = {
    //  val current = Seq((indent * depth) + tr.value(node))
    //  val children = tr.childen(node).flatMap(c => k(c, depth + 1))

    //  current ++ children
    // }
    // and then k(node).mkString("\n")

    def f(node: A, depth: Int = 0): String = tr
      .childen(node)
      .foldLeft(((indent * depth) + tr.value(node) + "\n")) { case (acc, n) =>
        acc + f(n, depth + 1)
      }

    // remove last newline
    f(node).dropRight(1)
  }

  def preOrderToStringNoRoot[A](root: A, indent: String = "\t")(implicit
      tr: TreeNode[A]
  ): String = {
    def f(node: A, depth: Int = 0): String = tr
      .childen(node)
      .foldLeft(((indent * depth) + tr.value(node) + "\n")) { case (acc, n) =>
        acc + f(n, depth + 1)
      }

    tr.childen(root)
      .foldLeft("") { case (acc, child) => acc + f(child) }
      // remove last newline
      .dropRight(1)
  }
}
