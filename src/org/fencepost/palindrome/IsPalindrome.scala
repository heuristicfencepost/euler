package org.fencepost.palindrome

object IsPalindrome {

  // Absolutely must use conservative matching and the backref here
  val palindromePattern = """(\d)(\d*?)\1""".r

  // Basic test for a palindrome
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

  def byStringBasic(n:String):Boolean = {

    // Base case; empty string and single characters are by definition palindromes.
    // Place this test up front so that we can handle input values of a single
    // character.
    if (n.length == 0 || n.length == 1)
      return true
    if (n.head != n.last)
      false
    else
      byStringBasic(n.substring(1,n.length - 1))
  }

  private def toList(arg:Int):List[Int] = if (arg <= 9) List(arg) else toList(arg / 10) ::: List(arg % 10)

  private def byIntHelper(arg:List[Int]):Boolean = {

    if (arg.length == 0 || arg.length == 1)
      return true
    if (arg.head != arg.last)
      false
    else
      byIntHelper(arg.slice(1,arg.length - 1))
  }

  def byInt(n:Int):Boolean = {

    // Simple optimization; if we're dealing with a single digit return true
    // automatically.  No point in building a list in this case.
    if (n <= 9)
      return true

    byIntHelper(toList(n))
  }

  private def byIntMatchHelper(arg:List[Int]):Boolean = {

    arg match {
      case List() => true
      case List(_) => true
      case arghead :: rest if arg.last == arghead => byIntMatchHelper(rest.slice(0,rest.length - 1))
      case _ => false
    }
  }

  def byIntMatch(n:Int):Boolean = {

    // Simple optimization; if we're dealing with a single digit return true
    // automatically.  No point in building a list in this case.
    if (n <= 9)
      return true

    byIntMatchHelper(toList(n))
  }
}
