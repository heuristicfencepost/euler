package org.fencepost

import scala.math.max

// Use the pattern-matching integer-based predicate for now
import org.fencepost.palindrome.IsPalindrome.{byIntMatch => isPalindrome}

// Also need implicit conversion, Int => String and Int => List[Int]
import org.fencepost.palindrome.IsPalindrome.int2list
import org.fencepost.palindrome.IsPalindrome.int2string

object Euler4 {

  // Multiply the input term by all values equal to or less than that term.
  // Return the largest palindrome (if any) created by these multiplications.
  def findPalindrome(term1:Int):Option[Int] = {

    if (term1 == 1)
      return None
    val terms = (term1.to(1,-1)) toStream
    val rv = terms.map(_*term1).find { v => isPalindrome(v) }
    rv match {

      case None => findPalindrome(terms.tail.head)
      case x => x
    }
  }

  def main(args: Array[String]): Unit = {

    // If we assume a palindrome > 900000 exists then this value must also end with a 9, meaning
    // it can only be odd.  We thus need only consider half the possible sample set.  This could
    // be revised if no palindrome beginning with 9 were found.
    var result = (0 /: 999.to(1,-2)) { (curr,candidate) =>

      // A simple optimization; if the square of the candidate value is less than
      // our current max there's no point in continuing; since the candidate times a
      // smaller value will always be less than the candidate squared there's no
      // way it will be able to exceed the current max.
      if ((candidate ^ 2) < curr)
        curr
      findPalindrome(candidate) match {

        case Some(x) => max(curr,x)
        case None => curr
      }
    }
    println("Result: " + result)
  }
}
