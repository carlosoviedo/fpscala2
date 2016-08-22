package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      Math.pow(b(), 2) - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val currentDelta = delta()
      if (currentDelta < 0) Set()
      else {
        val sqrtDelta = Math.sqrt(currentDelta)
        val currentB = b()
        val denom = 2 * a()
        Set((-currentB + sqrtDelta) / denom, (-currentB - sqrtDelta) / denom)
      }
    }
  }
}
