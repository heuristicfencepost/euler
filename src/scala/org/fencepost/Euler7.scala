package org.fencepost

// Sometimes mutability does allow for a cleaner expression of an algorithm
object Euler7 {

  def main(args: Array[String]): Unit = {

    // Pre-populate our prime list with some known quantities
    var primes = List(2,3,5,7,11,13,17,19)
    var count = 21
    while (primes.length < 10001) {

      // Use primes.tail instead of primes here; no need to compare to 2
      // since we're only considering odd candidates.
      if (primes.tail.filter(count % _ == 0).length == 0)
        primes = primes ::: List(count)
      count += 2
    }
    println("Prime 10001: " + primes.last)
  }
}
