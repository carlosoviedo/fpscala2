package week1

abstract class JSON {
  override def toString = this match {
    case JSeq(elems) => s"[${elems mkString ", "}]"
    case JObj(bindings) =>
      val assocs = bindings map {
        case (key, value) => s"""\"$key\": ${value}"""
      }
      s"{${assocs mkString ", "}}"
    case JNum(num) => num.toInt.toString
    case JStr(str) => s"""\"$str\""""
    case JBool(b) => b.toString
    case JNull => "null"
  }
}
case class JSeq (elems: List[JSON]) extends JSON
case class JObj (bindings: Map[String, JSON]) extends JSON
case class JNum (num: Double) extends JSON
case class JStr (str: String) extends JSON
case class JBool (b: Boolean) extends JSON
case object JNull extends JSON
