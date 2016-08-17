package week4

import scala.util.{Failure, Success, Try}

trait Future[T] { self =>
  def onComplete(callback: Try[T] => Unit): Unit = ???
  def flatMap[S](f: T => Future[S]): Future[S] =
    new Future[S] {
      override def onComplete(callback: (Try[S]) => Unit): Unit =
        self onComplete {
          case Success(x) => f(x) onComplete callback
          case Failure(e) => callback(Failure(e))
        }
    }
}
