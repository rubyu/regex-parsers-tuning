
package com.github.rubyu.parsertuning

import util.parsing.combinator.RegexParsers


trait Parser extends RegexParsers {
  override val skipWhitespace = false
  lazy val line: Parser[Result.Element] = rowElement <~ eol
  lazy val lastLine: Parser[Result.Element] = rowElement <~ eof

  lazy val rowElement: Parser[Result.Element] = row ^^ { x => Result.Row(if (x.size == 1 && x(0).isEmpty) List[String]() else x) }

  def row: Parser[List[String]]
  def eol: Parser[String]
  val eof: Parser[String] = """\z""".r
}
