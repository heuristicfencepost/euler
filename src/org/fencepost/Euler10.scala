package org.fencepost

object Euler10 {

  def nat(n:Long):Stream[Long] = Stream.cons(n,nat(n + 1))

  def sieve(s: Stream[Long]): Stream[Long] =
   Stream.cons(s.head, sieve(s.tail filter { _ % s.head != 0 }))

  def primes = sieve(nat(2))

  def main(args: Array[String]): Unit = {

    val result = (0L /: (primes takeWhile { _ < 10 })) (_+_)
    println("Result: " + result)
  }
}
