package me.qingkou.u24
import org.scalatest.FlatSpec
/**
 * Created by seeker on 4/25/2014.
 */
class ASpec extends FlatSpec {
  "Rational" should "work" in {
    val r1 = new Rational(2, 4)
    val r2 = new Rational(3, 6)
    assert(r1.numer == r2.numer)
    assert(r1.denom == r2.denom)
    assert(r1 == r2)
    assert(r1 equals r2)
    assert(!(r1 eq r2))
  }
  "Arith" should "work" in {
    val a  = new Arith[Int](7, 8, ADD)
    val aa = new Arith[Int](7, 8, ADD)
    val b  = new Arith[Int](7, 8, MULTIPLY)
    assert(a == aa)
    assert(a != b)
  }
  "TrackableRational" should "work" in {
    val t1  = new TrackableRational(2, 4, None)
    val t2  = new TrackableRational(1, 2, None)
    val one = new TrackableRational(2, 2, None)
    val two = new TrackableRational(2, 1, None)
    val t3  = new TrackableRational(2, 1, Some(new Arith[TrackableRational](one, one, ADD)))
    val t4  = new TrackableRational(2, 1, Some(new Arith[TrackableRational](one, new TrackableRational(3, 3, None), ADD)))
    // Should the 2 equal?
    assert(t1 == t2)
    assert(t3 == t4)
    assert(two != t4)

  }
}
