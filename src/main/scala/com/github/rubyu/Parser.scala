
package com.github.rubyu.parsertuning

import util.parsing.combinator.RegexParsers


trait Parser extends RegexParsers {
  override val skipWhitespace = false

  lazy val line          : Parser[Result.Element] = ls  ^^^ Result.Row(Nil) | row <~ ls
  lazy val lastLine      : Parser[Result.Element] = eof ^^^ Result.Row(Nil) | row <~ eof | invalidString
  lazy val row           : Parser[Result.Element] = repsep( field, fs ) ^^ { Result.Row(_) }
  lazy val invalidString : Parser[Result.Element] = """.+""".r ^^ { Result.InvalidString(_) }

  def field : Parser[String]
  def fs    : Parser[String]
  def ls    : Parser[String]
  val eof   : Parser[String] = """\z""".r
}
