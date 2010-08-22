package org.fencepost.palindrome

import org.scalatest.Suite

// Make sure the implicit conversions are in scope as well
import IsPalindrome.int2list
import IsPalindrome.int2string

class IsPalindromeTest extends Suite {

  // Original implementation used doTest() method, however
  // List[Int] => Boolean cannot be substituted for Int => Boolean
  // even if there is an implicit conversion in play from
  // Int to List[Int].  We don't necessarily want to pollute the
  // API to work around a dynamic testing issue, however, so
  // fall back to the more verbose methods shown here.
  private def doTest(f:Int => Boolean) {

    assert(f(1))
    assert(f(11))
    assert(! f(12))
    assert(f(111))
    assert(f(191))
    assert(f(1991))
    assert(! f(1921))
    assert(! f(1992))
  }

  //def testByInt() = doTest(IsPalindrome.byInt)
  //def testByIntMatch() = doTest(IsPalindrome.byIntMatch)

  def testByRegex() = {
    
    assert(IsPalindrome.byRegex(1))
    assert(IsPalindrome.byRegex(11))
    assert(! IsPalindrome.byRegex(12))
    assert(IsPalindrome.byRegex(111))
    assert(IsPalindrome.byRegex(191))
    assert(IsPalindrome.byRegex(1991))
    assert(! IsPalindrome.byRegex(1921))
    assert(! IsPalindrome.byRegex(1992))
  }

  def testByString() = {

    assert(IsPalindrome.byString(1))
    assert(IsPalindrome.byString(11))
    assert(! IsPalindrome.byString(12))
    assert(IsPalindrome.byString(111))
    assert(IsPalindrome.byString(191))
    assert(IsPalindrome.byString(1991))
    assert(! IsPalindrome.byString(1921))
    assert(! IsPalindrome.byString(1992))
  }

  def testByInt() = {

    assert(IsPalindrome.byInt(1))
    assert(IsPalindrome.byInt(11))
    assert(! IsPalindrome.byInt(12))
    assert(IsPalindrome.byInt(111))
    assert(IsPalindrome.byInt(191))
    assert(IsPalindrome.byInt(1991))
    assert(! IsPalindrome.byInt(1921))
    assert(! IsPalindrome.byInt(1992))
  }

  //def testByIntMatch() = doTest(IsPalindrome.byIntMatch)
  def testByIntMatch() = {

    assert(IsPalindrome.byIntMatch(1))
    assert(IsPalindrome.byIntMatch(11))
    assert(! IsPalindrome.byIntMatch(12))
    assert(IsPalindrome.byIntMatch(111))
    assert(IsPalindrome.byIntMatch(191))
    assert(IsPalindrome.byIntMatch(1991))
    assert(! IsPalindrome.byIntMatch(1921))
    assert(! IsPalindrome.byIntMatch(1992))
  }
}
