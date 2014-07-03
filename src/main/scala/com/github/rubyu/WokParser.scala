
package com.github.rubyu.parsertuning

import java.io
import java.lang.CharSequence
import annotation.tailrec
import util.matching.Regex
import util.parsing.combinator.RegexParsers


case class QuoteOption(Q: Option[Char] = None, E: Option[Char] = None, P: Option[Regex] = None) {
  def withQuote(Q: Char) = this.copy(Q=Some(Q))
  def withEscape(E: Char) = this.copy(E=Some(E))
  def withPattern(P: Regex) = this.copy(P=Some(P))
}

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

  class Parser1(val FS: Regex, val RS: Regex, val Q: Char, val E: Char) extends Parser {

    /*
    case (All, Q, E) => quote all, escape Q with E
     */

    //  長さ0以上の文字列。
    lazy val field        : Parser[String] = quoted_field

    // Qで囲まれていること。Qの前後の空白は無視される。
    lazy val quoted_field : Parser[String] = " *".r ~> Q ~> quoted_value <~ Q <~ " *".r

    //　EQ => Q, EE => E のみエスケープ。EとQは単独で出現してはならない。
    lazy val quoted_value : Parser[String] = rep( E ~> Q | E ~> E | s"""((?!$Q)(?!$E).)+""".r ) ^^ { _.mkString }

  }

  class Parser2(val FS: Regex, val RS: Regex, val Q: Char, val E: Char) extends Parser {

    /*
    case (Min, Q, E) => quote if contains Q, escape Q with E
     */

    //  長さ0以上の文字列。
    lazy val field        : Parser[String] = quoted_field | raw_field

    // Qで囲まれていること。Qの前後の空白は無視される。
    lazy val quoted_field : Parser[String] = " *".r ~> Q ~> quoted_value <~ Q <~ " *".r

    //　EQ => Q, EE => E のみエスケープ。EとQは単独で出現してはならない。
    lazy val quoted_value : Parser[String] = rep( E ~> Q | E ~> E | s"""((?!$Q)(?!$E).)+""".r ) ^^ { _.mkString }

    //  先頭にQ、FS、RSが出現してはならない。FSとRS以外が後続する。
    lazy val raw_field    : Parser[String] = s"""(((?!$FS)(?!$RS)(?!$Q).)((?!$FS)(?!$RS).)*)?""".r

  }

  class Parser3(val FS: Regex, val RS: Regex) extends Parser {

    /*
    case (None) => quote nothing
     */

    //  長さ0以上の文字列。
    lazy val field       : Parser[String] = non_escaped

    //  先頭にFS、RS以外からなる文字列。
    lazy val non_escaped : Parser[String] = s"""(((?!$FS)(?!$RS).)*)?""".r

  }

  class Parser4(val FS: Regex, val RS: Regex, val E: Char) extends Parser {

    /*
    case (None, E) => quote match(FS|RS) with E
     */

    //  長さ0以上の文字列。
    lazy val field   : Parser[String] = escaped

    //  エスケープされたE, FS, RS, またはE, FS, RS以外からなる文字列。
    lazy val escaped : Parser[String] = rep( E ~> E | E ~> FS | E ~> RS | s"""((?!$E)(?!$FS)(?!$RS).)+""".r ) ^^ { _.mkString }

  }

  /*
  in: InputStream
  CD //codec
  FS //field separator pattern  //空白の時は？　空白NGで、 (?!.). とかにすれば絶対にマッチしないのでいいんでは
  RS //row separator pattern  //空白はNG
  FQ = Quote.All | Quote.Min | Quote.None withQuote withEscape withFilter
        addFilter のほうがいい？ addFilter("\r\n") →順序が保証されない

  case (All, Q, E) => quote all, escape Q with E

    field        : Parser[String] = quoted_field
    // Qで囲まれていること。Qの前後の空白は無視される。
    quoted_field : Parser[String] = " *$Q".r ~> quoted_value <~ "$Q *".r
    //　EQ => Q, EE => E のみエスケープ。EとQは単独で出現してはならない。
    quoted_value : Parser[String] = rep( E ~ Q ^^^ Q | E ~ E ^^^ E | """((?!$Q)(?!$E).)+""".r ) ^^ { _.mkString }

  case (Min, Q, E) => quote if contains Q, escape Q with E

    field        : Parser[String] = quoted_field | raw_field
    // Qで囲まれていること。Qの前後の空白は無視される。
    quoted_field : Parser[String] = " *$Q".r ~> quoted_value <~ "$Q *".r
    //　EQ => Q, EE => E のみエスケープ。EとQは単独で出現してはならない。
    quoted_value : Parser[String] = rep( E ~ Q ^^^ Q | E ~ E ^^^ E | """((?!$Q)(?!$E).)+""".r ) ^^ { _.mkString }
    //  先頭にQ、FS、RSが出現してはならない。FSとRS以外が後続する。
    raw_field    : Parser[String] = s"""(((?!$FS)(?!$LS)(?!$Q).)((?!$FS)(?!$RS).)*)?""".r

  case (None) => quote nothing

    field        : Parser[String] = raw_field
    //  先頭にFS、RS以外からなる文字列。
    raw_field    : Parser[String] = s"""(((?!$FS)(?!$RS).)*)?""".r

  case (None, E) => quote match(FS|RS) with E

    field        : Parser[String] = escaped
    //  エスケープされたE, FS, RS, またはE, FS, RS以外からなる文字列。
    escaped = rep( E ~ E ^^^ E | E ~ FS ^^^ FS | E ~ RS ^^^ RS | """((?!$E)(?!$FS)(?!$RS).)+""".r ) ^^ { _.mkString }

  case (None, E, P) => quote match(P) with E
    //parserは同上。


   */


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
