import scala.concurrent.Future

def retry[T](noTimes: Int)(block: => Future[T]): Future[T] = {
  val times = (1 to noTimes).toList
  val attempts = times map (_ => () => block)
  val failed: Future[T] = Future.failed(new Exception("Sorry"))

  (attempts foldLeft failed)((future, block) => future recoverWith {case e: Throwable => block()})
}