import week1.{Generator, Inner, Leaf, Tree}
import java.util.Random

val integers = new Generator[Int] {
  val rand = new Random
  def generate = rand.nextInt
}

val booleans = new Generator[Boolean] {
  def generate = integers.generate > 0
}

val pairs = new Generator[(Int, Int)] {
  def generate = (integers.generate, integers.generate)
}

val booleans2 = for (x <- integers) yield x > 0
def pairs2[T, U](t: Generator[T], u: Generator[U]) =
  for {
    x <- t
    y <- u
  } yield (x, y)

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate: T = x
}

def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- integers)
    yield if (x < 0) lo - x % (hi - lo) else lo + x % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)

def lists: Generator[List[Int]] = for {
  isEmpty <- booleans2
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)
def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

def trees: Generator[Tree] = for {
  isLeaf <- booleans2
  tree <- if (isLeaf) leafTree else nonLeafTree
} yield tree

def leafTree = for (x <- integers) yield Leaf(x)
def nonLeafTree = for {
  left <- trees
  right <- trees
} yield Inner(left, right)

def test[T](g: Generator[T], numTimes: Int = 100)
           (test: T => Boolean): Unit = {
  for (i <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), s"test failed for: $value")
  }
  println(s"passed $numTimes tests")
}

booleans.generate
booleans2.generate
pairs.generate
pairs2(integers, booleans).generate

val fruitsGenerator = oneOf("apple", "orange", "banana", "pineapple")
fruitsGenerator.generate
fruitsGenerator.generate
lists.generate
trees.generate

test(pairs2(lists, lists)) {
  case (xs, ys) => (xs ++ ys).length >= xs.length
}