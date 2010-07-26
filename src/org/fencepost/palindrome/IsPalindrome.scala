package org.fencepost.palindrome

object IsPalindrome {

  // Utility method to convert an input int into a list of bytes, one for each base 10
  // integer making up the input list.  For example 123 would be converted to List(1,2,3)
  private def toList(arg:Int):List[Int] = if (arg <= 9) List(arg) else toList(arg / 10) ::: List(arg % 10)

  // Tail-call version of toList above.  Note that this returns the list in inverse order so
  // we implement this as a helper; the actual predicate below will handle the reversal.
  private def toListTCHelper(arg:Int):List[Int] = if (arg <= 9) List(arg) else arg % 10 :: toListTCHelper(arg / 10)

  private def toListTC(arg:Int):List[Int] = toListTCHelper(arg).reverse

  // Pattern to be used by the regex-based predicate.  Absolutely must use conservative
  // matching and the backref here to make this work.
  val palindromePattern = """(\d)(\d*?)\1""".r

  // Recursive helper function to check for a palindrome using a regex
  def byRegexHelper(n:String):Boolean = {

    // Base case; empty string and single characters are by definition palindromes.
    // Place this test up front so that we can handle input values of a single
    // character.
    if (n.length == 0 || n.length == 1)
      return true
    palindromePattern.unapplySeq(n) match {

      case Some(matches) => byRegexHelper(matches(1))
      case None => false
    }
  }

  // Regex-based predicate; convert to a string and call the recrusive function
  def byRegex(arg:Int):Boolean = byRegexHelper(arg.toString)

  // Recursive helper function to perform the check based on comparisons of the
  // head and last characters in a string
  def byStringHelper(n:String):Boolean = {

    // Base case; empty string and single characters are by definition palindromes.
    // Place this test up front so that we can handle input values of a single
    // character.
    if (n.length == 0 || n.length == 1)
      return true
    if (n.head != n.last)
      false
    else
      byStringHelper(n.substring(1,n.length - 1))
  }

  // String-based predicate; convert to string and call the recursive function
  def byString(arg:Int):Boolean = byStringHelper(arg.toString)

  // Recursive helper function to check for a palindrome by comparing the first
  // and last integers in a list
  private def byIntHelper(arg:List[Int]):Boolean = {

    if (arg.length == 0 || arg.length == 1)
      return true
    if (arg.head != arg.last)
      false
    else
      byIntHelper(arg.slice(1,arg.length - 1))
  }

  // Simple integer-based predicate; convert argument into a list of Integers
  // and call recursive function.
  def byInt(n:Int):Boolean = {

    // Simple optimization; if we're dealing with a single digit return true
    // automatically.  No point in building a list in this case.
    if (n <= 9)
      return true

    byIntHelper(toListTC(n))
  }

  // Recursive helper function to check for palindromes using only pattern matching.
  private def byIntMatchHelper(arg:List[Int]):Boolean = {

    // Note that we don't need to check for lists of length 0 or length 1 as we do
    // in byIntHelper above.  The first two cases of our match operation below handle
    // these cases.
    arg match {
      case List() => true
      case List(_) => true
      case arghead :: rest if arg.last == arghead => byIntMatchHelper(rest.slice(0,rest.length - 1))
      case _ => false
    }
  }

  // Integer-based predicate using pattern matching; convert argument into a list
  // and call recursive function.
  def byIntMatch(n:Int):Boolean = {

    // Simple optimization; if we're dealing with a single digit return true
    // automatically.  No point in building a list in this case.
    if (n <= 9)
      return true

    byIntMatchHelper(toListTC(n))
  }
}
