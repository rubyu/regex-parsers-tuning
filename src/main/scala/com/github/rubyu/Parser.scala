
package com.github.rubyu.parsertuning

import util.parsing.combinator.RegexParsers


trait Parser extends RegexParsers {
  override val skipWhitespace = false
  lazy val line: Parser[Result.Element] = eol ^^^ Result.Row(Nil) | rowElement <~ eol
  lazy val lastLine: Parser[Result.Element] = eof ^^^ Result.Row(Nil) | rowElement <~ eof | invalidString

  lazy val invalidString: Parser[Result.Element] = """.+""".r ^^ { Result.InvalidString(_) }
  lazy val rowElement: Parser[Result.Element] = row ^^ { Result.Row(_) }

  def row: Parser[List[String]]
  def eol: Parser[String]
  val eof: Parser[String] = """\z""".r
}
