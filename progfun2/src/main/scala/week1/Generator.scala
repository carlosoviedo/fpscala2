package week1

trait Generator[T] {
  self =>         // defines an alias for "this" === Generator.this

  def generate: T

  def map[S](f: T => S): Generator[S] = flatMap(f andThen Generator.apply)

  def flatMap[S](f: T => Generator[S]): Generator[S] = f(self.generate)
}

object Generator {
  def apply[T](x: T) = new Generator[T] {
    def generate = x
  }
}