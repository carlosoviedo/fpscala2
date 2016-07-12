import scala.annotation.tailrec

def last[T](xs: List[T]): T = xs match {
  case Nil => throw new NoSuchElementException("last of empty list")
  case x :: Nil => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case Nil => throw new NoSuchElementException("init of empty list")
  case x :: Nil => Nil
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case Nil => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case Nil => xs
  case y :: ys => reverse(ys) ::: y :: Nil
}

def reverseTR[T](xs: List[T]): List[T] = {
  @tailrec
  def loop(ys: List[T], acc: List[T]): List[T] = ys match {
    case Nil => acc
    case z :: zs => loop(zs, z :: acc)
  }

  loop(xs, Nil)
}

def removeAt[T](xs: List[T], n: Int): List[T] = {
  @tailrec
  def loop(ys: List[T], i: Int, acc: List[T]): List[T] = ys match {
    case Nil => xs
    case z :: zs =>
      if (i == 0) acc ++ zs
      else loop(zs, i-1, acc ++ List(z))
  }

  if (n < 0) xs
  else loop(xs, n, Nil)
}

def removeAt2[T](xs: List[T], n: Int) = (xs take n) ++ (xs drop n+1)

def flatten[T <: Any](xs: List[Any]): List[Any] = xs match {
  case Nil => xs
  case y :: ys => y match {
    case Nil => flatten(ys)
    case z: List[T] => flatten(z) ::: flatten(ys)
    case z: Any => z :: flatten(ys)
  }
}

val x = List('a', 'b', 'c', 'd')
reverse(x)
reverseTR(x)
removeAt(x, 1)
removeAt2(x, 1)
flatten(List(List(1, 1), 2, List(3, List(5, 8))))