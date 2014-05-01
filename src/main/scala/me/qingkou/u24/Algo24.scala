package me.qingkou.u24

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Created by seeker on 4/27/2014.
 */
class Algo24(val goal: TrackableRational, val input: Seq[TrackableRational]) {

  var output: Set[TrackableRational] = Set()

  def execute0(in: Seq[TrackableRational]): Unit = {

    def expand(tr1: TrackableRational, tr2: TrackableRational): ArrayBuffer[TrackableRational] = {
      def expand0(t1: TrackableRational, t2: TrackableRational): ArrayBuffer[TrackableRational] = {
        val s = ArrayBuffer[TrackableRational]()
        s += (t1 + t2)
        s += (t1 - t2)
        s += (t1 * t2)
        s += (t2 - t1)
        if (t2.rational.numer != 0) s += (t1 / t2)
        if (t1.rational.numer != 0) s += (t2 / t1)
        s
      }
      if (tr1 > tr2) {
        expand0(tr1, tr2)
      } else {
        expand0(tr2, tr1)
      }
    }

    if (in.length == 1) {
      if(in(0).rational == goal.rational) output += in(0)
      return
    } else {
      for(i <- 0 until in.length - 1){
        for(j <- i + 1 until in.length) {
          val s: ArrayBuffer[TrackableRational] = expand(in(i), in(j))
          for(f <- s) {
            val b = ListBuffer[TrackableRational]()
            in.copyToBuffer(b)
            b remove j
            b remove i
            b += f
            execute0(b.toSeq)
          }
        }
      }
    }
  }

  def execute() = execute0(input)
}
object main extends App{
  val ag = new Algo24(new TrackableRational(24), Array(new TrackableRational(8), new TrackableRational(8), new TrackableRational(3), new TrackableRational(3)))
  ag.execute()
  for(o <- ag.output) {
    println(o.toFinalString)
  }
}