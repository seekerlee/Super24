package me.qingkou.u24

/**
 * Created by seeker on 4/24/2014.
 */
class TrackableRational(val rational: Rational, val arith: Option[Arith[TrackableRational]]) extends Ordered[TrackableRational] {

  def this(n: Int, d: Int, arith: Option[Arith[TrackableRational]]) = this(new Rational(n, d), arith)
  def this(n: Int, d: Int) = this(n, d, None)
  def this(n: Int) = this(n, 1)
  def +(that: TrackableRational) = new TrackableRational(rational + that.rational, Some(new Arith[TrackableRational](this, that, ADD)))
  def -(that: TrackableRational) = new TrackableRational(rational - that.rational, Some(new Arith[TrackableRational](this, that, SUBTRACT)))
  def *(that: TrackableRational) = new TrackableRational(rational * that.rational, Some(new Arith[TrackableRational](this, that, MULTIPLY)))
  def /(that: TrackableRational) = new TrackableRational(rational / that.rational, Some(new Arith[TrackableRational](this, that, DIVIDE)))

  override def toString = arith match {
    case None     => rational.toString
    case Some(ar) => "(" + ar.lValue + " " + ar.op + " " + ar.rValue + ")"
  }

  def toFinalString = arith match {
    case None     => toString
    case Some(ar) => ar.lValue + " " + ar.op + " " + ar.rValue + " = " + rational
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[TrackableRational]

  override def equals(other: Any): Boolean = other match {
    case that: TrackableRational =>
      (that canEqual this) &&
        rational == that.rational &&
        ((arith, that.arith) match {
          case (None, None) => true
          case (Some(ar1), Some(ar2)) => ar1 == ar2
          case _ => false
        })
    case _ => false
  }

  override def hashCode(): Int = arith match {
      case Some(ar) => rational.hashCode * 31 + ar.hashCode
      case None     => rational.hashCode
  }

  override def compare(that: TrackableRational): Int = this.rational compare that.rational
}
object TrackableRational {
  def ZERO = new TrackableRational(0)
}