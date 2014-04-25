package me.qingkou.u24

/**
 * Created by seeker on 4/24/2014.
 */
class Oprator
object ADD      extends Oprator {override def toString = "+"}
object SUBTRACT extends Oprator {override def toString = "-"}
object MULTIPLY extends Oprator {override def toString = "*"}
object DIVIDE   extends Oprator {override def toString = "/"}

class Arith[T](val lValue: T, val rValue: T, val op: Oprator) {

  private def canEqual(other: Any): Boolean = other.isInstanceOf[Arith[T]]

  override def equals(other: Any): Boolean = other match {
    case that: Arith[T] =>
      (that canEqual this) &&
        lValue == that.lValue &&
        rValue == that.rValue &&
        op == that.op
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(lValue, rValue, op)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
