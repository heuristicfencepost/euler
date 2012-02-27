package org.fencepost

object Euler1 {

  def main(args: Array[String]): Unit = {

    val l = (0 until 1000).filter { x => x % 3 == 0 || x % 5 == 0 }
    val r = (0 /: l) (_ + _)
    println (r)
  }
}
