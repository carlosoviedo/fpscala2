def WHILE(condition: => Boolean)(command: => Unit): Unit =
  if (condition) {
    command
    WHILE(condition)(command)
  }
else ()

def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (condition) ()
  else REPEAT(command)(condition)
}

class REPEAT2(command: => Unit) {
  def UNTIL(condition: => Boolean): Unit = REPEAT(command)(condition)
}

object REPEAT2 {
  def apply(command: => Unit): REPEAT2 = new REPEAT2(command)
}

REPEAT(println("from repeat"))(true)
REPEAT{println("from repeat again")}(true)

REPEAT2 {
  println("from enhanced repeat")
} UNTIL true