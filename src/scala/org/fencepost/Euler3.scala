package org.fencepost

import scala.math.sqrt

object Euler3 {

  def nat(n:Long):Stream[Long] = Stream.cons(n,nat(n + 1))

  def sieve(s: Stream[Long]): Stream[Long] =
   Stream.cons(s.head, sieve(s.tail filter { _ % s.head != 0 }))

  def primes = sieve(nat(2))

  def factorization(candidates:Stream[Long],target:Long,thusfar:List[Long]):List[Long] = {

    if ((1L /: thusfar)(_*_) == target) { thusfar }
    else {

      val head = candidates.head
      factorization(candidates.tail,target,if (target % head == 0) thusfar ::: List(head) else thusfar)
    }
  }

  def main(args: Array[String]): Unit = {

    val target = 600851475143L

    // We only need search through sqrt(target) at most.  Wikipedia's article on "trial division" (as a
    // factorization method) offers the following explanation:
    //
    // "Furthermore, the trial factors need go no further than  because, if n is divisible by some number p,
    // then n = p×q and if q were smaller than p, n would have earlier been detected as being divisible by
    // q or a prime factor of q"
    // (http://en.wikipedia.org/wiki/Trial_division)
    println(factorization(primes takeWhile (_ < sqrt(target)),target,List[Long](1)).last)
  }
}
