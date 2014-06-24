
package com.github.rubyu.parsertuning

import java.io
import java.lang.CharSequence
import annotation.tailrec
import com.github.rubyu.parsertuning.Reader3.{JointCharSequence, FastCharSequence}


class Reader4(parser: Parser, in: io.Reader) extends Reader {

  private def read(until: Int): CharSequence = {
    val buf = new Array[Char](until)
    in.read(buf) match {
      case -1 => ""
      case n => new FastCharSequence(buf, 0, n)
    }
  }

  private def fillBuffer(maxIncrement: Int): Boolean = {
    read(maxIncrement) match {
      case s if s.length == 0 => false
      case s if buffer.length == 0 => buffer = s; true
      case s => buffer = new JointCharSequence(buffer, s); true
    }
  }

  private val minBufferSize = 1048576 //1MB
  private val minReadSize = 512000 //500KB
  private var buffer: CharSequence = ""
  private var hasRemaining = true

  private def parseNext(): Option[Result.Element] = {
    @tailrec
    def _parseNext(canBeLast: Boolean = false): Option[Result.Element] = {
      buffer.length match {
        case 0 if !hasRemaining => None
        case _ =>
          if (hasRemaining && buffer.length < minBufferSize) {
            hasRemaining = fillBuffer(minBufferSize - buffer.length + minReadSize)
          }
          parser.parse(if (canBeLast) parser.lastLine else parser.line, buffer) match {
            case x if x.successful =>
              buffer = buffer.subSequence(x.next.offset, buffer.length)
              Some(x.get)
            case x =>
              if (hasRemaining) {
                hasRemaining = fillBuffer(buffer.length)
              }
              _parseNext(!hasRemaining)
          }
      }
    }
    _parseNext()
  }

  private var _next: Option[Result.Element] = None

  def hasNext = {
    _next match {
      case Some(x) => true
      case None => _next = parseNext(); _next.isDefined
    }
  }

  def next() = {
    _next match {
      case Some(x) => _next = None; x
      case None => parseNext().getOrElse(throw new NoSuchElementException)
    }
  }

}


object Reader4 {
  class JointCharSequence(a: CharSequence, b: CharSequence) extends CharSequence {
    lazy val length = a.length + b.length

    def charAt(i: Int) = {
      if (i < 0 || i >= length) {
        throw new IndexOutOfBoundsException
      }
      if (i < a.length) {
        a.charAt(i)
      } else {
        b.charAt(i-a.length)
      }
    }

    def subSequence(s: Int, e: Int) = {
      if (s < 0 || e < 0 || s > e || e > length) {
        throw new IndexOutOfBoundsException
      }
      if (s < a.length) {
        if (e <= a.length) {
          a.subSequence(s, e)
        } else {
          val _e = e-a.length
          val _a = if (s == 0) a else a.subSequence(s, a.length)
          val _b = if (_e == b.length) b else b.subSequence(0, _e)
          new JointCharSequence(_a, _b)
        }
      } else {
        b.subSequence(s-a.length, e-a.length)
      }
    }

    override def toString(): String = a.toString + b.toString

    override def equals(other: Any) = other match {
      case that: JointCharSequence if that.isInstanceOf[JointCharSequence] => toString == that.toString
      case _ => false
    }
  }

  /**
   * https://issues.scala-lang.org/browse/SI-7710
   */
  class FastCharSequence(chars: Array[Char], val sb: Int, val eb: Int) extends CharSequence {
    def this(chars: Array[Char]) = this(chars, 0, chars.length)

    lazy val length = eb - sb

    def charAt(i: Int): Char = {
      if (i < 0 || i >= length) {
        throw new IndexOutOfBoundsException
      }
      chars(i + sb)
    }

    def subSequence(s: Int, e: Int): CharSequence = {
      if (s < 0 || e < 0 || s > e || e > length) {
        throw new IndexOutOfBoundsException
      }
      new FastCharSequence(chars, sb + s, sb + e)
    }

    override def toString(): String = new String(chars, sb, length)
  }
}
