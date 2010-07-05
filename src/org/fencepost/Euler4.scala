package org.fencepost

import scala.math.max

object Euler4 {

  val palindromePattern = """(\d)(\d*?)\1""".r

  // Basic test for a palindrome
  def isPalindrome(n:String):Boolean = {

    palindromePattern.unapplySeq(n) match {

      case Some(matches) =>
        if (matches(1).length == 0 || matches(1).length == 1) true else isPalindrome(matches(1))
      case None => false
    }
  }

  // Multiply the input term by all values equal to or less than that term.
  // Return the largest palindrome (if any) created by these multiplications.
  def findPalindrome(term1:Int):Option[Int] = {

    //println("findPalindrome, term1: " + term1)
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

    // If we assume a palindrome > 900000 exists then this value must also end with a 9, meaning
    // it can only be odd.  We thus need only consider half the possible sample set.  This could
    // be revised if no palindrome beginning with 9 were found.
    var result = (0 /: 999.to(1,-2)) { (curr,candidate) =>

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
