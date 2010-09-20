package org.fencepost

object Euler9 {

  // Modification of Fibonacci definition employing principle that abs(a^2 - b^2) = a + b if
  // abs(a - b) = 1
  lazy val squares:Stream[Long] = Stream.cons(0,squares.zipWithIndex.map { case (a,b) => a + b + b + 1 } toStream)

  def main(args: Array[String]): Unit = {

    val result =
      for {
        i <- 1 to 999
        j <- (i + 1) to 1000
        if (i + j) < 1000 // to avoid out of bounds errors when we check for c
        c = (1000 - i - j)
        if squares(i) + squares(j) == squares(c)
      } yield i * j * c
    println("Result: " + result)
  }
}
