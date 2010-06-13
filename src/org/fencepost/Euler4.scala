package org.fencepost

object Euler4 {

  def main(args: Array[String]): Unit = {

    def isPalindrome(n:String):Boolean = {

      val len = n.length
      if (len <= 1)
        return false

      // Strings of size 1 are by definition equivalent backwards and forwards
      //if (len == 1)
      //  return true

      // We now know we've got at least two characters in the string.  If they
      // don't match there's no point in going on
      if (n.charAt(0) != n.charAt(len - 1))
        return false

      // Anything larger than exactly two characters needs a recursive call
      if (len > 2)
        return isPalindrome(n.substring(1,len - 1))

      // At this point we have a string of exactly two identical characters.
      // This is the base case for our recursive definition of isPalindrome()
      true
    }

    def findPalindrome(candidates:Stream[Int]):Option[Int] = {

      if (candidates.size == 0)
        return None
      val rv = candidates.tail.map(_*candidates.head).find { v:Int => isPalindrome(v.toString()) }
      rv match {

        case None => findPalindrome(candidates.tail)
        case x => x
      }
    }

    def findMaxPalindrome(candidates:Stream[Int],max:Int):Int = {

      if (candidates == Stream.empty)
        return 0
      println("findMaxPalindrome starting with " + candidates.head)
      findPalindrome(candidates) match {

        case Some(x) => return Math.max(max,Math.max(x,findMaxPalindrome(candidates.tail,max)))
        case None => return Math.max(max,findMaxPalindrome(candidates.tail,max))
      }
    }

    val candidates =
      for { i <- 999.to(1,-2)}
        yield i
    println("Result: " + findMaxPalindrome(candidates toStream,0))
  }
}
