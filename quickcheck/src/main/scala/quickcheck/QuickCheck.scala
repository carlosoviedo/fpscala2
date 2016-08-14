package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of empty heap") = forAll { (a1: Int, a2: Int) =>
    findMin(insert(a1, insert(a2, empty))) == min(a1, a2)
  }

  property("insert and delete from empty heap") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("sorted sequence") = forAll { (h: H) =>
    def toList(h2: H): List[Int] = {
      def go(remaining: H, acc: List[Int]): List[Int] =
        if (isEmpty(remaining)) acc.reverse
        else {
          val m = findMin(remaining)
          go(deleteMin(remaining), m :: acc)
        }
      go(h2, Nil)
    }

    def isSorted(l: List[Int]): Boolean = l match {
      case Nil => false
      case x :: Nil => true
      case x :: y :: xs =>
        if (y < x) false
        else isSorted(l.tail)
    }

    val seq = toList(h)
    seq.isEmpty && isEmpty(h) || isSorted(seq)
  }

  property("min of melding two heaps") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = findMin(meld(h1, h2))
    m == m1 || m == m2
  }

  property("meld with empty") = forAll { (h: H) =>
    val h1 = meld(h, empty)
    h1 == h
  }

  property("heap with 2 equal mins") = forAll { (a: Int) =>
    val h = insert(a, insert(a, empty))
    val m = findMin(h)

    val h2 = deleteMin(h)
    val m2 = findMin(h2)
    a == m && a == m2 && isEmpty(deleteMin(h2))
  }

  property("findMin") = forAll { (a1: Int, a2: Int, a3: Int) =>
    val list = List(a1, a2, a3).sorted
    val lowest = list.head
    val lowest2 = list.tail.head
    val lowest3 = list.tail.tail.head

    val h = insert(a1, insert(a2, insert(a3, empty)))
    val m = findMin(h)

    val h2  = deleteMin(h)
    val m2 = findMin(h2)

    val h3 = deleteMin(h2)
    val m3 = findMin(h3)

    m == lowest && m2 == lowest2 && m3 == lowest3
  }
}