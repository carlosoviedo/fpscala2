import week4.{BankAccountFRP, Signal}

class Position(x: Int, y: Int) {
  def <=(pos: Position): Boolean = ???
}

def mousePosition: Signal[Position] =
  Signal[Position]{new Position(2,4)}

def inRectangle(LL: Position, UR: Position): Signal[Boolean] =
  Signal[Boolean]{
    val pos:Position = mousePosition()
    LL <= pos && pos <= UR
  }

val sig = Signal(3)

def consolidated(accts: List[BankAccountFRP]): Signal[Int] =
  Signal((accts map (_.currentBalance)).sum)

val a,b = new BankAccountFRP
val c = consolidated(List(a,b))
c()
a deposit 20
c()
b deposit 30
c()
val xchange = Signal(246.00)
val inDollar = Signal(c() * xchange())
inDollar()
b withdraw 10
c()
inDollar()