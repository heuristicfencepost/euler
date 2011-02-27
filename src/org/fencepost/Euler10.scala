package org.fencepost

import scala.math.sqrt
import scala.math.floor

object Euler10 {

  def nat(n:Long):Stream[Long] = Stream.cons(n,nat(n + 1))

  // Predicate to test for primality.  Base test is to verify that a candidate isn't evenly
  // divisible into all integers between 2 and the candidate's square root.  As an optimization
  // we can compare all primes within this range.  We pursue this path here, however in order
  // to make the whole thing work we must supply an initial "seed" value of the first prime;
  // thus the Stream.cons() usage below.
  def pred(n:Long):Boolean = {

      val root = floor(sqrt(n))
      (primes takeWhile { _ <= root }) forall { n % _ != 0 }
  }

  // Note that since we're explicitly stating 2 as the first argument to Stream.cons()
  // we now have to start the natural number sequence at 3.
  lazy val primes:Stream[Long] = Stream.cons(2,nat(3) filter pred)

  def main(args: Array[String]): Unit = {

    println("Result: " + (0L /: (primes takeWhile { _ < 2000000 })) (_+_))
  }
}
