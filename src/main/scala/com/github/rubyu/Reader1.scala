
package com.github.rubyu.parsertuning

import java.io
import collection.mutable.ListBuffer
import annotation.tailrec


class Reader1(parser: Parser, in: io.Reader) extends Reader {

  @tailrec
  private def read(until: Int, i: Int = 0, buffer: ListBuffer[Char] = new ListBuffer[Char]): String = {
    if (i >= until) {
      buffer.mkString
    } else {
      in.read() match {
        case -1 => buffer.mkString
        case n => buffer += n.toChar; read(until, i+1, buffer)
      }
    }
  }

  private var buffer = ""

  private def parseNext(): Option[Result.Element] = {
    @tailrec
    def _parseNext(last: Boolean = false): Option[Result.Element] = {
      val reachEnd = read(100000) match {
        case s if s.isEmpty => true
        case s => buffer += s; false
      }
      buffer match {
        case buf if buf.isEmpty && reachEnd => None
        case buf =>
          /*
           RegexParsers.scala has O(inputlength) memory performance on java >= 7u6
           https://issues.scala-lang.org/browse/SI-7710
           parser.parse( , buf <- here!)
           */
          parser.parse(if (last) parser.lastLine else parser.line, buf) match {
            case x if x.successful =>
              buffer = buf.drop(x.next.offset)
              x.get match {
                case elem: Result.EOL => _parseNext()
                case elem => Some(elem)
              }
            case x =>
              last match {
                case true => try { Some(Result.InvalidString(buf)) } finally { buffer = "" }
                case false => if (reachEnd) _parseNext(true) else _parseNext()
              }
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