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

  def preOrderToString[A](node: A, indent: String = "\t")(implicit
      tr: TreeNode[A]
  ): String = {
    def f(node: A, depth: Int = 0): String = tr
      .childen(node)
      .foldLeft(((indent * depth) + tr.value(node) + "\n")) { case (acc, n) =>
        acc + f(node, depth + 1)
      }

    f(node)
  }
}
