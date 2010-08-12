package org.fencepost.palindrome

object IsPalindrome {

  // Utility method to convert an input int into a list of integers, one for each digit in
  // the base 10 representation of the input int.  For example 123 would be converted to
  // List(1,2,3)
  private def int2list(arg:Int):List[Int] = if (arg <= 9) List(arg) else int2list(arg / 10) ::: List(arg % 10)

  // Tail-call version of toList above.  Note that this returns the list in inverse order so
  // we implement this as a helper; the actual conversion function below will handle the reversal.
  private def int2listTCHelper(arg:Int):List[Int] = if (arg <= 9) List(arg) else arg % 10 :: int2listTCHelper(arg / 10)

  implicit def int2listTC(arg:Int):List[Int] = int2listTCHelper(arg).reverse

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
