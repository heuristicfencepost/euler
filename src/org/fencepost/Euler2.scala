package org.fencepost

object Euler2 {

  def main(args: Array[String]): Unit = {

    // Hat tip to Daniel S. for the clever Fib computation:
    // http://www.codecommit.com/blog/scala/infinite-lists-for-the-finitely-patient
    //
    // Key thing to realize is that fibs.tail will always be one element smaller
    // than fibs and zip() only takes the min if inputs are of different sizes.  This
    // means the map function returns the sum of 0 and 1, 1 and 2 and so on.  This
    // is equivalent to item 3, 4 and so on in the Fibonacci series.
    lazy val fibs:Stream[Int] = Stream.cons(0,Stream.cons(1,fibs.zip(fibs.tail).map { case (a, b) => a + b }))
    val fibsSub = fibs takeWhile { _ < 4000000 } filter { _ % 2 == 0}
    println((0 /: fibsSub )(_ + _))
  }
}
