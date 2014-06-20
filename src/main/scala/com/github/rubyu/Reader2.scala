
package com.github.rubyu.parsertuning

import java.io
import java.lang.CharSequence
import annotation.tailrec


class Reader2(parser: Parser, in: io.Reader) extends Reader {

  private def read(until: Int): CharSequence = {
    val buf = new Array[Char](until)
    in.read(buf) match {
      case -1 => ""
      case n => new FastCharSequence(buf, 0, n)
    }
  }

  private var buffer: CharSequence = ""
  private var reachEnd = false

  private def parseNext(): Option[Result.Element] = {
    @tailrec
    def _parseNext(last: Boolean = false): Option[Result.Element] = {
      if (buffer.length == 0 && reachEnd) {
        None
      } else {
        parser.parse(if (last) parser.lastLine else parser.line, buffer) match {
          case x if x.successful =>
            buffer = buffer.subSequence(x.next.offset, buffer.length)
            Some(x.get)
          case x =>
            reachEnd = read(math.max(100000, buffer.length)) match {
              case s if s.length == 0 => true
              case s if buffer.length == 0 => buffer = s; false
              case s => buffer = new FastCharSequence(buffer.toString + s); false
            }
            _parseNext(reachEnd)
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

  /**
   * todo 複数のArray[Char]を保持するように
   * https://issues.scala-lang.org/browse/SI-7710
   */
  class FastCharSequence(chars: Array[Char], val startBounds: Int, val endBounds: Int) extends CharSequence {
    def this(chars: Array[Char]) = this(chars, 0, chars.length)
    def this(input: String)      = this(input.toCharArray)

    def length(): Int = endBounds - startBounds

    def charAt(index: Int): Char = {
      if (index < length) {
        chars(index + startBounds)
      } else {
        throw new IndexOutOfBoundsException(s"$boundsInfo index: $index")
      }
    }

    def subSequence(start: Int, end: Int): CharSequence = {
      if (start >= 0 && start <= length && end >= 0 && end <= length) {
        new FastCharSequence(chars, startBounds + start, startBounds + end)
      } else {
        throw new IndexOutOfBoundsException(s"$boundsInfo start: $start, end $end")
      }
    }

    override def toString(): String = new String(chars, startBounds, length)

    private def boundsInfo = s"current: (startBounds: $startBounds, endBounds: $endBounds, length: $length, chars length: ${chars.length})"
  }
}

