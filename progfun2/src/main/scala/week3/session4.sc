class Wire
val a,b,c = new Wire

def inverter(input: Wire, output: Wire): Unit
def andGate(a1: Wire, a2: Wire, output: Wire): Unit
def orGate(a1: Wire, a2: Wire, output: Wire): Unit

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

def f(a: Wire, b: Wire, c: Wire): Unit = {
  val d, e, f, g = new Wire
  inverter(a, d)    // d = ~a
  inverter(b, e)    // e = ~b
  andGate(a, e, f)  // f = a & e == a & ~b
  andGate(b, d, g)  // g = b & d == b & ~a
  orGate(f, g, c)   // c = f | g == (a & ~b) | (b & ~a) (a != b)
}