package xyz.bluepitaya.common

import scala.annotation.tailrec

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
  def preOrderToString[A](
      node: A,
      indent: String = "\t",
      additionalStr: String = ""
  )(implicit tr: TreeNode[A]): String = {
    // def k(node: A, depth: Int = 0): Seq[String] = {
    //  val current = Seq((indent * depth) + tr.value(node))
    //  val children = tr.childen(node).flatMap(c => k(c, depth + 1))

    //  current ++ children
    // }
    // and then k(node).mkString("\n")

    def f(node: A, depth: Int = 0): String = tr
      .childen(node)
      .foldLeft((additionalStr + (indent * depth) + tr.value(node) + "\n")) {
        case (acc, n) => acc + f(n, depth + 1)
      }

    // remove last newline
    f(node).dropRight(1)
  }

  def preOrderToStringNoRoot[A](
      root: A,
      indent: String = "\t",
      additionalStr: String = ""
  )(implicit tr: TreeNode[A]): String = {
    def f(node: A, depth: Int = 0): String = tr
      .childen(node)
      .foldLeft((additionalStr + (indent * depth) + tr.value(node) + "\n")) {
        case (acc, n) => acc + f(n, depth + 1)
      }

    tr.childen(root)
      .foldLeft("") { case (acc, child) => acc + f(child) }
      // remove last newline
      .dropRight(1)
  }

  def preOrderListMap[A, B](root: A, mapFunc: A => B)(implicit
      tr: TreeNode[A]
  ): List[B] = {
    def f(node: A): List[B] = {
      val mapped = mapFunc(node)
      List(mapped) ++ tr.childen(node).map(child => f(child)).flatten
    }

    f(root)
  }

  def map[A, B](
      root: A,
      noChildrenMap: A => B,
      setChildren: (B, Seq[B]) => B
  )(implicit tra: TreeNode[A], trb: TreeNode[B]): B = {
    case class QueueObj[A](node: A, depth: Int, processed: Boolean)
    case class MappedObj[B](node: B, depth: Int)

    @tailrec
    def traverse(
        queue: Seq[QueueObj[A]],
        childrenQueue: List[MappedObj[B]],
        // To avoid taking head from childrenQueue, we ensure result is always present
        result: B
    ): B = queue match {
      case QueueObj(node, depth, processed) :: rest =>
        if (!processed) {
          val updatedQueueObj = QueueObj(node, depth, true)
          val childrenQueueObjs = tra
            .childen(node)
            .map(n => QueueObj(n, depth + 1, false))
          val nextQueue = childrenQueueObjs ++ (updatedQueueObj :: rest)
          traverse(nextQueue, childrenQueue, result)
        } else {
          val (children, restChildrenQueue) = childrenQueue
            .span(obj => obj.depth > depth)
          val mappedNode = MappedObj(
            setChildren(noChildrenMap(node), children.reverse.map(_.node)),
            depth
          )
          val nextChildrenQueue = mappedNode :: restChildrenQueue
          traverse(rest, nextChildrenQueue, mappedNode.node)
        }
      case Nil => result
    }

    traverse(Seq(QueueObj(root, 0, false)), List(), noChildrenMap(root))
  }

  def postorderList[A](root: A)(implicit tr: TreeNode[A]): List[A] = {
    case class QueueObj[A](node: A, processed: Boolean)

    @tailrec
    def traverse(queue: Seq[QueueObj[A]], result: List[A]): List[A] =
      queue match {
        case QueueObj(node, processed) :: rest =>
          if (processed) { traverse(rest, node :: result) }
          else {
            val cjildrenQueueObjs = tr
              .childen(node)
              .map(n => QueueObj(n, false))
            val nextQueue = cjildrenQueueObjs ++ (QueueObj(node, true) :: rest)
            traverse(nextQueue, result)
          }
        case Nil => result
      }

    traverse(Seq(QueueObj(root, false)), List())
  }
}
