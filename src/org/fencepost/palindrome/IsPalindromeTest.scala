package org.fencepost.palindrome

import org.scalatest.Suite

class IsPalindromeTest extends Suite {

  def testByRegex() {

    assert(IsPalindrome.byRegex(""))
    assert(IsPalindrome.byRegex("1"))
    assert(IsPalindrome.byRegex("11"))
    assert(! IsPalindrome.byRegex("12"))
    assert(IsPalindrome.byRegex("111"))
    assert(IsPalindrome.byRegex("191"))
    assert(IsPalindrome.byRegex("1991"))
    assert(! IsPalindrome.byRegex("1921"))
    assert(! IsPalindrome.byRegex("1992"))
  }

  def testByString() {

    assert(IsPalindrome.byStringBasic(""))
    assert(IsPalindrome.byStringBasic("1"))
    assert(IsPalindrome.byStringBasic("11"))
    assert(! IsPalindrome.byStringBasic("12"))
    assert(IsPalindrome.byStringBasic("111"))
    assert(IsPalindrome.byStringBasic("191"))
    assert(IsPalindrome.byStringBasic("1991"))
    assert(! IsPalindrome.byStringBasic("1921"))
    assert(! IsPalindrome.byStringBasic("1992"))
  }

  def testByInt() {

    assert(IsPalindrome.byInt(1))
    assert(IsPalindrome.byInt(11))
    assert(! IsPalindrome.byInt(12))
    assert(IsPalindrome.byInt(111))
    assert(IsPalindrome.byInt(191))
    assert(IsPalindrome.byInt(1991))
    assert(! IsPalindrome.byInt(1921))
    assert(! IsPalindrome.byInt(1992))
  }

  def testByIntMatch() {

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
