package org.fencepost

object Euler2 {

  def main(args: Array[String]): Unit = {

    // Hat tip to Daniel S. for the clever Fib computation:
    // http://www.codecommit.com/blog/scala/infinite-lists-for-the-finitely-patient
    lazy val fibs:Stream[Int] = Stream.cons(0,Stream.cons(1,fibs.zip(fibs.tail).map { case (a, b) => a + b }))
    val fibsSub = fibs takeWhile { _ < 4000000 } filter { _ % 2 == 0}
    val t = (0 /: fibsSub )(_ + _)
    println(t)
  }
}
