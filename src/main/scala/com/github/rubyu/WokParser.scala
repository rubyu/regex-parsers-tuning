
package com.github.rubyu.parsertuning

import java.io
import java.lang.CharSequence
import annotation.tailrec
import util.matching.Regex
import util.parsing.combinator.RegexParsers

object Quote {

  trait QuoteMode

  case class QuoteAll(Q: Option[Char] = None, E: Option[Char] = None, P: Option[Regex] = None) extends QuoteMode {
    def withQuote(Q: Char) = this.copy(Q=Some(Q))
    def withEscape(E: Char) = this.copy(E=Some(E))
    def withPattern(P: Regex) = this.copy(P=Some(P))
  }

  case class QuoteMin(Q: Option[Char] = None, E: Option[Char] = None, P: Option[Regex] = None) extends QuoteMode {
    def withQuote(Q: Char) = this.copy(Q=Some(Q))
    def withEscape(E: Char) = this.copy(E=Some(E))
    def withPattern(P: Regex) = this.copy(P=Some(P))
  }

  case class QuoteNone(E: Option[Char] = None, P: Option[Regex] = None) extends QuoteMode {
    def withEscape(E: Char) = this.copy(E=Some(E))
    def withPattern(P: Regex) = this.copy(P=Some(P))
  }

  def All = QuoteAll() withQuote('"')
  def Min = QuoteMin() withQuote('"')
  def None = QuoteNone()
}


import Quote.{ QuoteMode, QuoteAll, QuoteMin, QuoteNone }


class WokParser(parser: Parser, in: io.Reader) extends Reader {

  trait Parser extends RegexParsers {
    override val skipWhitespace = false

    lazy val line : Parser[Result.Element] = (RS | EOF)  ^^^ Result.Row(Nil) | row <~ (RS | EOF)
    lazy val row  : Parser[Result.Element] = repsep( field, FS ) ^^ { Result.Row(_) }

    def field : Parser[String]
    def FS    : Regex
    def RS    : Regex
    def EOF   : Regex = """\z""".r
  }

  case class ParserImpl(FS: Regex, RS: Regex, QM: QuoteMode) extends Parser {

    lazy val field : Parser[String] = QM match {
      case QuoteAll (Some(q), Some(e), _) => quoted( q, text(q, e) )                 // quote all, and escape Q with E
      case QuoteAll (Some(q),    None, _) => quoted( q, text(q) )                    // quote all, and escape nothing
      case QuoteMin (Some(q), Some(e), _) => quoted( q, text(q, e) ) | non_quoted(e) // quote if contains Q, and escape Q with E
      case QuoteMin (Some(q),    None, _) => quoted( q, text(q) )    | non_quoted    // quote if contains Q, and escape nothing
      case QuoteNone(         Some(e), _) => non_quoted(e)                           // escape (E|FS|RS) with E
      case QuoteNone(            None, _) => non_quoted                              // escape nothing
    }

    //  Quoteで囲まれていること。
    def quoted(Q: Char, T: Parser[String]) : Parser[String] = Q ~> T <~ Q
    //  QuoteでエスケープされたQuoteか、Quote以外からなる、長さ0以上の文字列。
    def text(Q: Char)                      : Parser[String] = rep( Q ~> Q | s"""((?!$Q).)+""".r ) ^^ { _.mkString }
    //  EscapeされたEscape・Quote・FS・RSか、Escape・Quote以外からなる、長さ0以上の文字列。
    def text(Q: Char, E: Char)             : Parser[String] = rep( E ~> E | E ~> Q | E ~> FS | E ~> RS | s"""((?!$Q)(?!$E).)+""".r ) ^^ { _.mkString }
    //  EscapeされたEscape・FS・RSか、Escape・FS・RS以外からなる、長さ0以上の文字列。
    def non_quoted(E: Char)                : Parser[String] = rep( E ~> E | E ~> FS | E ~> RS | s"""((?!$E)(?!$FS)(?!$RS).)+""".r ) ^^ { _.mkString }
    //  FS・RS以外からなる、長さ0以上の文字列。
    def non_quoted                         : Parser[String] = s"""(((?!$FS)(?!$RS).)*)?""".r
  }

  private def read(until: Int): CharSequence = {
    val buf = new Array[Char](until)
    in.read(buf) match {
      case -1 => ""
      case n => new WokParser.FastCharSequence(buf, 0, n)
    }
  }

  private var buffer: CharSequence = ""
  private var reachEnd = false

  private def parseNext(): Option[Result.Element] = {
    @tailrec
    def _parseNext(canBeLast: Boolean = false): Option[Result.Element] = {
      buffer.length match {
        case 0 if reachEnd => None
        case _ =>
          parser.parse(if (canBeLast) parser.lastLine else parser.line, buffer) match {
            case x if x.successful =>
              buffer = buffer.subSequence(x.next.offset, buffer.length)
              Some(x.get)
            case x =>
              if (!reachEnd) {
                reachEnd = read(math.max(1000000, buffer.length)) match {
                  case s if s.length == 0 => true
                  case s if buffer.length == 0 => buffer = s; false
                  case s => buffer = new WokParser.JointCharSequence(buffer, s); false
                }
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

}


object WokParser {
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
