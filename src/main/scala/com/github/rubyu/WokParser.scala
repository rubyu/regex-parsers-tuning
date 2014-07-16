
package com.github.rubyu.parsertuning.wok

import java.io
import io.{InputStream, PrintStream}
import java.lang.CharSequence
import annotation.tailrec
import util.matching.Regex
import util.parsing.combinator.RegexParsers
import java.nio.charset.StandardCharsets


object WokParser {

  trait QuoteMode
  case object QuoteAll extends QuoteMode
  case object QuoteMin extends QuoteMode
  case object QuoteNone extends QuoteMode

  case class QuoteOption(M: QuoteMode=QuoteNone, Q: Option[Char] = None, E: Option[Char] = None) {
    def All() = this.copy(M=QuoteAll, Q=if (Q.isDefined) Q else Some('"'))
    def Min() = this.copy(M=QuoteMin, Q=if (Q.isDefined) Q else Some('"'))
    def None() = this.copy(M=QuoteNone)
    def Q(c: Char): QuoteOption = this.copy(Q=Some(c))
    def E(c: Char): QuoteOption = this.copy(E=Some(c))
  }

  def Quote() = QuoteOption()


  case class Row0(field: List[String], sep: List[String]) {
    def toRow1(term: String) = Row1(field, sep, term)
  }
  case class Row1(field: List[String], sep: List[String], term: String) {
    def toRow(id: Long) = Row(id, field, sep, term)
  }
  case class Row(id: Long, field: List[String], sep: List[String], term: String)


  implicit class EscapedRegexString(val s: String) extends AnyVal {
    def er: Regex = new Regex(s.replaceAll("""(\\|\*|\+|\.|\?|\{|\}|\(|\)|\[|\]|\^|\$|\-|\|)""", """\\$1"""))
  }

  implicit class EscapedRegexChar(val c: Char) extends AnyVal {
    def er: Regex = c.toString.er
  }


  class WokCsvParser(FS: Regex, RS: Regex, FQ: QuoteOption) extends RegexParsers {
    override val skipWhitespace = false

    /*
    # RFC4180
    * Each record is located on a separate line, delimited by a line break (CRLF).
    * The last record in the file may or may not have an ending line break.

    # Wok
    ## Relaxations
    * Records may be delimited by strings other than CRLF.
     */
    lazy val line: Parser[Row1] =
      row_empty | row ~ (RS | EOF) ^^
        { case row0 ~ term => row0.toRow1(term) }

    lazy val row_empty: Parser[Row1] =
      (RS | EOF) ^^
        { Row1(Nil, Nil, _) }

    /*
    # RFC4180
    * Within the header and each record, there may be one or more fields, separated by commas.
    * Each line should contain the same number of fields throughout the file.
    * Spaces are considered part of a field and should not be ignored.
    * The last field in the record must not be followed by a comma.

    # Wok
    ## Relaxations
    * Records may be empty.
    * Fields may be separated by strings other than commas.
    * Records may contain the variant number of fields.
     */
    lazy val row: Parser[Row0] =
      field ~ ( rep( FS ~ field ) ).? ^^
        {
          case first ~ Some(rest) => rest.map { case fs ~ f => (fs, f) }.unzip match { case (fs, f) => Row0(first +: f, fs) }
          case first ~ None       => Row0(List(first), Nil)
        }

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
    * Escape characters escape quote-characters, field-separators, line-separators and itself.
    * Fields enclosed with quote-characters and not containing any quote-character excluding those escaped by another
        quote-character or a escape-character are treated as quoted fields, otherwise the fields are treated as
        non-quoted fields.

    ## Relaxations
    * Characters other than double quote may be used as quote character.
    * Quote-characters, field-separators and line-separators, following right after escape-characters, may appear inside
        the fields both enclosed and not enclosed with quote-characters.
    * Non-quoted fields may contain quote-characters.
     */
    lazy val field : Parser[String] = FQ match {
      case QuoteOption(QuoteAll, Some(q), Some(e)) => quoted( q, text(q, e) )                 // quote all, and escape Q with E
      case QuoteOption(QuoteAll, Some(q),    None) => quoted( q, text(q) )                    // quote all, and escape nothing
      case QuoteOption(QuoteMin, Some(q), Some(e)) => quoted( q, text(q, e) ) | non_quoted(e) // quote if contains Q, and escape Q with E
      case QuoteOption(QuoteMin, Some(q),    None) => quoted( q, text(q) )    | non_quoted    // quote if contains Q, and escape nothing
      case QuoteOption(QuoteNone,      _, Some(e)) => non_quoted(e)                           // escape (E|FS|RS) with E
      case QuoteOption(QuoteNone,      _,    None) => non_quoted                              // escape nothing
    }

    /*
    Strings enclosed with quote-characters.
      */
    def quoted(Q: Char, T: Parser[String]): Parser[String] =
      Q ~> T <~ Q

    /*
    Quote-characters escaped with another quote-character or
      strings consist of characters other than quote-character.
    Larger and not equal to zero.
     */
    def text(Q: Char): Parser[String] =
      rep( Q ~> Q | s"""[^${Q.er}]+""".r ) ^^ { _.mkString }

    /*
    Quote-characters escaped with another quote-character or
      escape-characters, field-separators and line-separators escaped with escape-character or
      strings consist of characters other than quote-character and escape-character.
    Larger and not equal to zero.
     */
    def text(Q: Char, E: Char): Parser[String] =
      rep( Q ~> Q | E ~> s"""(${Q.er}|${E.er}|$FS|$RS)""".r | E ^^^ "" | s"""[^${Q.er}${E.er}]+""".r ) ^^ { _.mkString }

    /*
    Escape-characters, field-separators and line-separators escaped with escape-character or
      escape-characters or
      strings consist of strings other than field-separators and line-separators and
                         characters other than quote-character and escape-character.
    Larger and not equal to zero.
     */
    def non_quoted(E: Char): Parser[String] =
      rep( E ~> s"""(${E.er}|$FS|$RS)""".r | E ^^^ "" | s"""((?!$FS)(?!$RS)[^${E.er}])+""".r ) ^^ { _.mkString }

    /*
    Strings consist of strings other than field-separators and line-separators.
    Larger than zero.
     */
    def non_quoted: Parser[String] =
      s"""((?!$FS)(?!$RS).)*""".r

    def EOF: Regex = """\z""".r

    def parse(in: CharSequence): ParseResult[Row1] = parse(line, in)
  }

  class WokCsvReader {
    private var fs = """\s+""".r
    private var rs = """(\r\n|\r|\n)""".r
    private var fq = QuoteOption()
    private var parser = new WokCsvParser(fs, rs, fq)

    private def update() { parser = new WokCsvParser(fs, rs, fq) }

    def parse(in: CharSequence) = parser.parse(in)

    def FS = fs
    def RS = rs
    def FQ = fq

    def FS(r: Regex) = { fs = r; update(); this }
    def RS(r: Regex) = { rs = r; update(); this }

    def FS(s: String) = { fs = s.er; update(); this }
    def RS(s: String) = { rs = s.er; update(); this }

    def FQ(q: QuoteOption) = { fq = q; update(); this }

    def open(in: io.Reader) = new WokCsvRowIterator(in, this)
  }

  def Reader() = new WokCsvReader


  implicit class QuotedString(val str: String) extends AnyVal {
    def quoted(q: Char): String = q.toString + str + q.toString
  }

  implicit class EscapedString(val str: String) extends AnyVal {
    def escaped(e: Char): String = str.escaped(e.toString)
    def escaped(e: Char, t: Char): String = str.escaped(e.toString, t.toString)
    def escaped(e: Char, t: String): String = str.escaped(e.toString, t)
    def escaped(e: String, t: String): String = str.escaped(e).replace(t, e + t)
    def escaped(e: String): String = str.replace(e, e + e)
  }

  class Writer(OFS: String, ORS: String, OFQ: QuoteOption) {

    private lazy val escape: String => String = {
      OFQ match {
        //escapes e and q in a given string with e and quotes it with q
        case QuoteOption(QuoteAll, Some(q), Some(e)) => { _.escaped(e, q).quoted(q) }

        //quotes a given string with q
        case QuoteOption(QuoteAll, Some(q), None) => { _.escaped(q).quoted(q) }

        //escapes e and q in a given string with e and quotes it with q when it contains OFS or ORS
        case QuoteOption(QuoteMin, Some(q), Some(e)) => {
          case s if s.contains(OFS) || s.contains(ORS) => s.escaped(e, q).quoted(q)
          case s => s.escaped(e, q)
        }

        //escapes q in a given string with q and quotes it with q when it contains OFS or ORS or q
        case QuoteOption(QuoteMin, Some(q), None) => {
          case s if s.contains(OFS) || s.contains(ORS) || s.contains(q) => s.escaped(q).quoted(q)
          case s => s
        }

        //escapes OFS in a given string with e and throws an error when it contains ORS
        case QuoteOption(QuoteNone, _, Some(e)) => {
          case s if s.contains(ORS) => throw new RuntimeException
          case s => s.escaped(e, OFS)
        }

        //throws an error when a given string contains OFS or ORS
        case QuoteOption(QuoteNone, _, None) => {
          case s if s.contains(OFS) || s.contains(ORS) => throw new RuntimeException
          case s => s
        }
      }
    }

    //todo support codec
    def write(out: PrintStream, x: Any) {
      val str = x match {
        case x: Seq[_] => x.map { x => escape(x.toString) } mkString(OFS)
        case x => escape(x.toString)
      }
      out.write(str.getBytes(StandardCharsets.UTF_8))
      out.write(ORS.getBytes(StandardCharsets.UTF_8))
    }
  }


  class WokCsvRowIterator(in: io.Reader, parser: WokCsvReader) extends Iterator[Row] {

    private def read(until: Int): CharSequence = {
      val buf = new Array[Char](until)
      in.read(buf) match {
        case -1 => ""
        case n => new FastCharSequence(buf, 0, n)
      }
    }

    private var resultId = -1

    private var buffer: CharSequence = ""
    private var reachEnd = false

    private def parseNext(): Option[Row] = {
      @tailrec
      def _parseNext(canBeLast: Boolean = false): Option[Row] = {
        buffer.length match {
          case 0 if reachEnd => None
          case _ =>
            parser.parse(buffer) match {
              /*
              Parser possibly returns false-positive Success() when Reader hasn't reach to the in's end and
                result.next.size is zero.

              | Buffer  | Rest |
              |---------|------|
              | "a,b,c" | ""   | => Success
              | "a,b"   | ",c" | => false-positive Success
              */
              case x if x.successful && (reachEnd || !x.next.atEnd) =>
                resultId += 1
                buffer = buffer.subSequence(x.next.offset, buffer.length)
                Some(x.get.toRow(resultId))
              case x if canBeLast =>
                throw new RuntimeException(s"""parse failed at line ${resultId + 2}, after ${buffer.subSequence(0, 10)}""")
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

    private var _next: Option[Row] = None

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
