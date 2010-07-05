package org.fencepost

import scala.math.max

object Euler4 {

  def main(args: Array[String]): Unit = {

    // Basic test for a palindrome
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

    // Find any palindrome containing the head of the current candidate list as one
    // of the two terms involved.  Do so by multiplying the head by all smaller
    // values and returning the first palindrome we find (since the first combination
    // of the head of the candidate list and another value in the candidate list
    // must be the largest possible value containing the candidate if we're moving
    // in descending order... which we are by virtue of the recursion).'
    def findPalindrome(candidates:Stream[Int]):Option[Int] = {

      if (candidates.size == 0)
        return None
      val rv = candidates.tail.map(_*candidates.head).find { v:Int => isPalindrome(v.toString()) }
      rv match {

        case None => findPalindrome(candidates.tail)
        case x => x
      }
    }

    // Driving recursive function: return a palindrome built from the current 
    // candidate list or whatever comes back from the recursive call, whichever
    // is larger.
    def findMaxPalindrome(candidates:Stream[Int],curr:Int):Int = {

      if (candidates == Stream.empty)
        return 0

      // A nice pruning optimization; if the square of the current candidate head is less
      // than the current max then there's no way we can beat it by multiplying the head
      // times a smaller value.
      if ((candidates.head ^ 2) < curr)
        return 0
      println("findMaxPalindrome starting with " + candidates.head)
      findPalindrome(candidates) match {

        case Some(x) => return max(curr,max(x,findMaxPalindrome(candidates.tail,curr)))
        case None => return max(curr,findMaxPalindrome(candidates.tail,curr))
      }
    }

    // If we assume a palindrome > 900000 exists then this value must also end with a 9, meaning
    // it can only be odd.  We thus need only consider half the possible sample set.  This could
    // be revised if no palindrome beginning with 9 were found.
    val candidates =
      for { i <- 999.to(1,-2)}
        yield i
    println("Result: " + findMaxPalindrome(candidates toStream,0))
  }
}
