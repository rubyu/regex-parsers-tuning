
package com.github.rubyu.parsertuning.wok

import java.io
import java.lang.CharSequence
import annotation.tailrec
import util.matching.Regex
import util.parsing.combinator.RegexParsers

object Quote {

  trait QuoteMode

  case class QuoteAll(Q: Option[Char] = scala.None, E: Option[Char] = scala.None, P: Option[Regex] = scala.None) extends QuoteMode {
    def withQuote(Q: Char) = this.copy(Q=Some(Q))
    def withEscape(E: Char) = this.copy(E=Some(E))
    def withPattern(P: Regex) = this.copy(P=Some(P))
  }

  case class QuoteMin(Q: Option[Char] = scala.None, E: Option[Char] = scala.None, P: Option[Regex] = scala.None) extends QuoteMode {
    def withQuote(Q: Char) = this.copy(Q=Some(Q))
    def withEscape(E: Char) = this.copy(E=Some(E))
    def withPattern(P: Regex) = this.copy(P=Some(P))
  }

  case class QuoteNone(E: Option[Char] = scala.None, P: Option[Regex] = scala.None) extends QuoteMode {
    def withEscape(E: Char) = this.copy(E=Some(E))
    def withPattern(P: Regex) = this.copy(P=Some(P))
  }

  def All = QuoteAll() withQuote('"')
  def Min = QuoteMin() withQuote('"')
  def None = QuoteNone()
}


import Quote.{ QuoteMode, QuoteAll, QuoteMin, QuoteNone }


object WokParser {

  case class Row0(field: List[String], sep: List[String]) {
    def toRow1(term: String) = Row1(field, sep, term)
  }
  case class Row1(field: List[String], sep: List[String], term: String) {
    def toRow(id: Long) = Row(id, field, sep, term)
  }
  trait Result
  case class Row(id: Long, field: List[String], sep: List[String], term: String) extends Result
  case class Error(remaining: CharSequence) extends Result

  trait Parser extends RegexParsers {
    override val skipWhitespace = false

    /*
    # RFC4180
    * Each record is located on a separate line, delimited by a line break (CRLF).
    * The last record in the file may or may not have an ending line break.

    # Wok
    ## Relaxations
    * Records may be delimited by strings other than a line break (CRLF).
     */
    lazy val line      : Parser[Row1] = empty_row | row ~ (RS | EOF) ^^ { case row0 ~ term => row0.toRow1(term) }
    lazy val empty_row : Parser[Row1] = (RS | EOF) ^^ { Row1(Nil, Nil, _) }

    /*
    # RFC4180
    *  Within the header and each record, there may be one or more fields, separated by commas.
    *  Each line should contain the same number of fields throughout the file.
    *  Spaces are considered part of a field and should not be ignored.
    *  The last field in the record must not be followed by a comma.

    # Wok
    ## Relaxations
    * Records may be empty.
    * Fields may be separated by strings other than commas.
    * Records may contain the variant number of fields.
     */
    lazy val row       : Parser[Row0] = field ~ ( rep( FS ~ field ) ).? ^^ {
      case first ~ Some(rest) => rest.map { case fs ~ f => (fs, f) }.unzip match { case (fs, f) => Row0(first +: f, fs) }
      case first ~ None       => Row0(List(first), Nil)
    }

    def field : Parser[String]
    def FS    : Regex
    def RS    : Regex
    def EOF   : Regex = """\z""".r

    def parse(in: CharSequence): ParseResult[Row1] = parse(line, in)
  }

  class ParserImpl(val FS: Regex, val RS: Regex, val QM: QuoteMode) extends Parser {

    /*
    # RFC4180
    * Each field may or may not be enclosed in double quotes (however some programs, such as Microsoft Excel, do not use
        double quotes at all).
    * If fields are not enclosed with double quotes, then double quotes may not appear inside the fields.
    * Fields containing line breaks (CRLF), double quotes, and commas should be enclosed in double-quotes.
    * If double-quotes are used to enclose fields, then a double-quote appearing inside a field must be escaped by
        preceding it with another double quote.

    # Wok
    ## Specifications
    * Escape characters escape Quote-characters, field-separators, line-separators and itself.

    ## Relaxations
    * Characters other than double quote may be used as quote character.
    * Quote-characters, field-separators and line-separators, following right after escape-characters, may appear inside
        the fields not enclosed with quote-characters.
     */
    lazy val field : Parser[String] = QM match {
      case QuoteAll (Some(q), Some(e), _) => quoted( q, text(q, e) )                 // quote all, and escape Q with E
      case QuoteAll (Some(q),    None, _) => quoted( q, text(q) )                    // quote all, and escape nothing
      case QuoteMin (Some(q), Some(e), _) => quoted( q, text(q, e) ) | non_quoted(e) // quote if contains Q, and escape Q with E
      case QuoteMin (Some(q),    None, _) => quoted( q, text(q) )    | non_quoted    // quote if contains Q, and escape nothing
      case QuoteNone(         Some(e), _) => non_quoted(e)                           // escape (E|FS|RS) with E
      case QuoteNone(            None, _) => non_quoted                              // escape nothing
    }

    /*
    String enclosed with quote-strings.
      */
    def quoted(Q: Char, T: Parser[String]): Parser[String] =
      Q ~> T <~ Q

    /*
    Quote-characters escaped with another quote-character or
      strings consist of characters other than quote-character.
    Larger and not equal to zero.
     */
    def text(Q: Char): Parser[String] =
      rep( Q ~> Q | s"""[^${rsafe(Q)}]+""".r ) ^^ { _.mkString }

    /*
    Quote-characters escaped with another quote-character or
      escape-characters, field-separators and line-separators escaped with escape-character or
      strings consist of characters other than quote-character and escape-character.
    Larger and not equal to zero.
     */
    def text(Q: Char, E: Char): Parser[String] =
      rep( Q ~> Q | E ~> s"""([${rsafe(Q)}${rsafe(E)}]|$FS|$RS)""".r | E ^^^ "" | s"""[^${rsafe(Q)}${rsafe(E)}]+""".r ) ^^ { _.mkString }

    /*
    Escape-characters, field-separators and line-separators escaped with escape-character or
      escape-characters or
      strings consist of strings other than field-separators and line-separators and
                         characters other than quote-character and escape-character.
    Larger and not equal to zero.
     */
    def non_quoted(E: Char): Parser[String] =
      rep( E ~> s"""([${rsafe(E)}]|$FS|$RS)""".r | E ^^^ "" | s"""((?!$FS)(?!$RS)[^${rsafe(E)}])+""".r ) ^^ { _.mkString }
    /*
    Strings consist of strings other than field-separators and line-separators.
    Larger than zero.
     */
    def non_quoted: Parser[String] =
      s"""((?!$FS)(?!$RS).)*""".r

    def rsafe(c: Char): String = c match {
      case '\\' => """\\"""
      case '*' | '+' | '.' | '?' | '{' | '}' | '(' | ')' | '[' | ']' | '^' |'$' | '-' | '|'  => """\""" + c.toString
      case _ => c.toString
    }
  }

  trait ParserOwner {
    def parser: Parser
  }

  trait AbstractWok extends ParserOwner {

  }


  class RowReader(in: io.Reader, owner: ParserOwner) extends Iterator[Result] {

    private def read(until: Int): CharSequence = {
      val buf = new Array[Char](until)
      in.read(buf) match {
        case -1 => ""
        case n => new FastCharSequence(buf, 0, n)
      }
    }

    private var buffer: CharSequence = ""
    private var reachEnd = false

    private def parseNext(): Option[Result] = {
      @tailrec
      def _parseNext(canBeLast: Boolean = false): Option[Result] = {
        buffer.length match {
          case 0 if reachEnd => None
          case _ =>
            owner.parser.parse(buffer) match {
              /*
              Parser may return false-positive Success() when not reachEnd and buffer is just the size of Row.
              | Buffer  | Rest |
              |---------|------|
              | "a,b,c" | ""   | => Success
              | "a,b"   | ",c" | => false-positive Success
              */
              case x if x.successful && (reachEnd || !x.next.atEnd) =>
                buffer = buffer.subSequence(x.next.offset, buffer.length)
                Some(x.get.toRow(0))
              case x if canBeLast =>
                Some(Error(buffer))
              case x =>
                if (!reachEnd) {
                  reachEnd = read(math.max(1000000, buffer.length)) match {
                    case s if s.length == 0 => true
                    case s if buffer.length == 0 => buffer = s; false
                    case s => buffer = new JointCharSequence(buffer, s); false
                  }
                }
                _parseNext(reachEnd)
            }
        }
      }
      _parseNext()
    }

    private var _next: Option[Result] = None

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
