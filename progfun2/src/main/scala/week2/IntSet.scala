package week2

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  override def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  override def union(other: IntSet): IntSet = other
}

case class NonEmpty(value: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean =
    value == x || (left contains x) || (right contains x)

  override def incl(x: Int): IntSet =
    if (x < value) NonEmpty(x, left incl x, right)
    else if (x > value) NonEmpty(x, left, right incl x)
    else this

  override def union(other: IntSet): IntSet = (left union (right union other)) incl value
}