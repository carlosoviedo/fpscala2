import scala.concurrent.Future

def retry[T](noTimes: Int)(block: => Future[T]): Future[T] = {
  if (noTimes == 0) Future.failed(new Exception("Sorry"))
  else block fallbackTo {
    retry(noTimes - 1){ block }
  }
}