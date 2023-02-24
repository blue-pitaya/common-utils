package xyz.bluepitaya.common

case class Range(from: Int, until: Int) {
  def size = until - from
  def contains(x: Int): Boolean = x >= from && x < until
}
