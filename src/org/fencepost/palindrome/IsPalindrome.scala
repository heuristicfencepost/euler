package org.fencepost.palindrome

import scala.collection.mutable.ListBuffer

object IsPalindrome {

  // Utility method to convert an input int into a list of integers, one for each digit in
  // the base 10 representation of the input int.  For example 123 would be converted to
  // List(1,2,3)

  // Naive implementation based on list concatenation
  //implicit def int2list(arg:Int):List[Int] = if (arg <= 9) List(arg) else int2list(arg / 10) ::: List(arg % 10)

  // Tail-call version of int2list above.  Using an accumulator array and the default argument
  // functionality introduced in Scala 2.8.0 allows us to implement this function without
  // a helper.
  //
  // Or at least it should; looks like algorithm for resolving implicit conversions doesn't
  // recognize default parameters... ?  The extra int2list declaration below is required
  // in order to make implicit conversions work correctly.  Attempts to use int2listTC()
  // directly for implicit conversion fails with type errors consistent with an environment
  // which had no implicit conversions.
  //def int2listTC(arg:Int, acc:List[Int] = List()):List[Int] = {
  //
  //  val newmember = arg % 10
  //  if (arg <= 9)
  //    acc ::: List(newmember)
  //  else
  //    int2listTC(arg / 10,acc ::: List(newmember))
  //}
  //
  //implicit def int2list(arg:Int) = int2listTC(arg)

  // The "stairway" book makes the point that the list concatenation operator
  // runs in O(n) where n is the size of the list on the left.  This does render
  // the naive implementation unnecessarily inefficient for very large integers,
  // especially when the ListBuffer class exists to solve this very problem.
  // It may not be functional but it does the job.
  implicit def int2list(arg:Int):List[Int] = {

    val buff = new ListBuffer[Int]
    var counter = arg
    while (counter > 9) {
      buff += (counter % 10)
      counter = (counter / 10)
    }
    buff += counter
    buff toList
  }

  // Add an implicit type conversion to make the string-based methods happy as well
  implicit def int2string(arg:Int):String = arg.toString

  // Pattern to be used by the regex-based predicate.  Absolutely must use conservative
  // matching and the backref here to make this work.
  val palindromePattern = """(\d)(\d*?)\1""".r

  // Recursive helper function to check for a palindrome using a regex
  def byRegex(n:String):Boolean = {

    // Base case; empty string and single characters are by definition palindromes.
    // Place this test up front so that we can handle input values of a single
    // character.
    if (n.length == 0 || n.length == 1)
      return true
    palindromePattern.unapplySeq(n) match {

      case Some(matches) => byRegex(matches(1))
      case None => false
    }
  }

  // Recursive helper function to perform the check based on comparisons of the
  // head and last characters in a string
  def byString(n:String):Boolean = {

    // Base case; empty string and single characters are by definition palindromes.
    // Place this test up front so that we can handle input values of a single
    // character.
    if (n.length == 0 || n.length == 1)
      return true
    if (n.head != n.last)
      false
    else
      byString(n.substring(1,n.length - 1))
  }
//
  // Recursive helper function to check for a palindrome by comparing the first
  // and last integers in a list
  def byInt(arg:List[Int]):Boolean = {

    if (arg.length == 0 || arg.length == 1)
      return true
    if (arg.head != arg.last)
      false
    else
      byInt(arg.slice(1,arg.length - 1))
  }

  // Recursive helper function to check for palindromes using only pattern matching.
  def byIntMatch(arg:List[Int]):Boolean = {

    // Note that we don't need to check for lists of length 0 or length 1 as we do
    // in byInt above.  The first two cases of our match operation below handle
    // these cases.
    arg match {
      case List() => true
      case List(_) => true
      case arghead :: rest if arg.last == arghead => byIntMatch(rest.slice(0,rest.length - 1))
      case _ => false
    }
  }
}
