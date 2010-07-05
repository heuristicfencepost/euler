package org.fencepost

import scala.math.max

object Euler4 {

  val palindromePattern = """(\d)(\d*?)\1""".r

  // Basic test for a palindrome
  def isPalindrome(n:String):Boolean = {

    if (n.length == 1)
      return true
    palindromePattern.unapplySeq(n) match {

      case Some(matches) =>
        if (matches(1).length == 0) true else isPalindrome(matches(1))
      case None => false
    }
  }

  // Multiply the input term by all values equal to or less than that term.
  // Return the largest palindrome (if any) created by these multiplications.
  def findPalindrome(term1:Int):Option[Int] = {

    println("findPalindrome, term1: " + term1)
    if (term1 == 1)
      return None
    val terms = (term1.to(1,-1)) toStream
    val rv = terms.map(_*term1).find { v => isPalindrome(v.toString()) }
    rv match {

      case None => findPalindrome(terms.tail.head)
      case x => x
    }
  }

  def main(args: Array[String]): Unit = {

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
      findPalindrome(candidates.head) match {

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
