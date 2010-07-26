package org.fencepost.palindrome

import org.scalatest.Suite

class IsPalindromeTest extends Suite {

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

  def testByRegex() = doTest(IsPalindrome.byRegex)

  def testByString() = doTest(IsPalindrome.byString)

  def testByInt() = doTest(IsPalindrome.byInt)

  def testByIntMatch() = doTest(IsPalindrome.byIntMatch)
}
