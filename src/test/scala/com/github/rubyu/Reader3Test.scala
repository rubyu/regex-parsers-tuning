
package com.github.rubyu.parsertuning

import org.specs2.mutable._
import Reader3.JointCharSequence


class Reader3Test extends SpecificationWithJUnit {
  /*
  def subSequence(s: Int, e: Int) = {
    if (s < 0 || e < 0 || s > e || e > length) {
      throw new IndexOutOfBoundsException
    }
    /**
     * str a = "abcd" length=4
     * str b = "efgh" length=4
     * s=1, e=3 => a.subSequence(s, e) (b,c)
     * s=3, e=5 => a.subSequence(s, a.length), b.subSequence(0, e-a.length)
     * s=5, e=7 => b.subSequence(s-a,length, e-(s-a.length))
     */
    if (s < a.length) {
      if (e < a.length) {
        a.subSequence(s, e)
      } else {
        val _e = e-a.length
        val _a = if (s == 0) a else a.subSequence(s, a.length)
        val _b = if (_e == b.length) b else b.subSequence(0, _e)
        new JointCharSequence(_a, _b)
      }
    } else {
      val _s = s-a.length
      b.subSequence(_s, e-_s)
    }
  }
   */

  "JointCharSequence.subString" should {
    "return CharSequence when start and end are in bounds" in {
      new JointCharSequence("a", "b")
        .subSequence(0, 0) mustEqual ""
      new JointCharSequence("a", "b")
        .subSequence(0, 1) mustEqual "a"
      new JointCharSequence("a", "b")
        .subSequence(0, 2) mustEqual new JointCharSequence("a", "b")
      new JointCharSequence("a", "b")
        .subSequence(1, 1) mustEqual ""
      new JointCharSequence("a", "b")
        .subSequence(1, 2) mustEqual "b"
    }
    "throw IndexOutOfBoundsException when start greater than end" in {
      new JointCharSequence("a", "b")
        .subSequence(1, 0) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(2, 0) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(2, 1) must throwA[IndexOutOfBoundsException]
    }
    "throw IndexOutOfBoundsException when start or end is out of bounds" in {
      new JointCharSequence("a", "b")
        .subSequence(-1, 0) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(-1, 1) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(-1, 2) must throwA[IndexOutOfBoundsException]

      new JointCharSequence("a", "b")
        .subSequence(0, 3) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(1, 3) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(2, 3) must throwA[IndexOutOfBoundsException]
    }
  }

  "JointCharSequence.charAt" should {

    "return Char when index in bounds" in {
      new JointCharSequence("a", "b")
        .charAt(0) mustEqual 'a'
      new JointCharSequence("a", "b")
        .charAt(1) mustEqual 'b'
    }

    "throw IndexOutOfBoundsException when index out of bounds" in {
      new JointCharSequence("a", "b")
        .charAt(-1) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .charAt(2) must throwA[IndexOutOfBoundsException]
    }
  }

  "JointCharSequence.length" should {

    "return a.legth + b.length" in {
      new JointCharSequence("a", "b")
        .length mustEqual 2
      new JointCharSequence("ab", "cd")
        .length mustEqual 4
    }
  }
}
