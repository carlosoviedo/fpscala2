package week3

abstract class Simulation {
  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private var curTime = 0
  def currentTime: Int = curTime

  private type Agenda = List[Event]
  private var agenda: Agenda = List()

  private def insert(a: Agenda, e: Event): Agenda = {
    val (earlier, later) = agenda span (_.time <= e.time)
    earlier ::: e :: later
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val newEvent = Event(curTime + delay, () => block)
    agenda = insert(agenda, newEvent)
  }

  def run: Unit = {
    afterDelay(0){
      println(s"*** simulation has started, time = $curTime ***")
    }
    loop()
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curTime = first.time
      first.action()
      loop
    case Nil =>
  }
}

abstract class Gates extends Simulation {
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal: Boolean = sigVal
    def setSignal(s: Boolean): Unit =
      if (s != sigVal) {
        sigVal = s
        actions foreach (_())
      }
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay){ output setSignal !inputSig}
    }
    input addAction invertAction
  }

  def andGate(input1: Wire, input2: Wire, output: Wire): Unit = {
    gate(input1, input2, output, AndGateDelay)(_&_)
  }

  def orGateAlt(input1: Wire, input2: Wire, output: Wire): Unit = {
    gate(input1, input2, output, OrGateDelay)(_|_)
  }

  def orGate(input1: Wire, input2: Wire, output: Wire): Unit = {
    val notIn1, notIn2, notOut = new Wire
    inverter(input1, notIn1)
    inverter(input2, notIn2)
    andGate(notIn1, notIn1, notOut)
    inverter(notOut, output)
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    wire addAction probeAction
  }

  private def gate(input1: Wire, input2: Wire, output: Wire, delay: Int)(f: (Boolean, Boolean) => Boolean) = {
    def gateAction(): Unit = {
      val inputSig1 = input1.getSignal
      val inputSig2 = input2.getSignal
      afterDelay(delay){ output setSignal f(inputSig1, inputSig2)}
    }
    input1 addAction gateAction
    input2 addAction gateAction
  }
}

abstract class Circuits extends Gates {
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s, c1, c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c2, c1, cout)
  }
}

trait Parameters {
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5
}

object MySimulation extends Circuits with Parameters {
}