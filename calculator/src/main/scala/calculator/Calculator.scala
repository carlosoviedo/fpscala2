package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map {case (key, value) => (key, Signal{eval(value(), namedExpressions - key)})}
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def op(op1: Expr, op2: Expr)(f: (Double, Double) => Double) = {
      f(eval(op1, references), eval(op2, references))
    }

    expr match {
      case Literal(v) => v
      case Plus(a, b) => op(a, b)(_+_)
      case Minus(a, b) => op(a, b)(_-_)
      case Times(a, b) => op(a, b)(_*_)
      case Divide(a, b) => op(a, b)(_/_)
      case Ref(name) => {
        val expr = getReferenceExpr(name, references)
        eval(expr, references - name)
      }
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
