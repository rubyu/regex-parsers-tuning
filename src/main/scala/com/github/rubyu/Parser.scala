
package com.github.rubyu.parsertuning

import util.parsing.combinator.RegexParsers


trait Parser extends RegexParsers {
  override val skipWhitespace = false
  lazy val line: Parser[Result.Element] = rowElement <~ eol | eolElement
  lazy val lastLine: Parser[Result.Element] = rowElement | eolElement

  def rowElement = row ^^ { Result.Row(_) }
  def eolElement = eol ^^ { Result.EOL(_) }

  def row: Parser[List[String]]
  def eol: Parser[String]
}
