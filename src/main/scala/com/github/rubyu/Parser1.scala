
package com.github.rubyu.parsertuning


class Parser1 extends Parser {

  //空のfieldがありえるので、sizeは1以上になる
  lazy val row: Parser[List[String]] = repsep( field, delim )

  //長さ0以上の文字列
  lazy val field         = quoted_field | raw_value

  //QUOTEに囲まれていること。前後にスペースによるパディングが存在してもよい。
  lazy val quoted_field    = padding ~> quote ~> quoted_value <~ quote <~ padding

  //QUOTE以外の文字, エスケープされたQUOTEからなる長さ0以上の文字列。
  lazy val quoted_value    = rep( escaped_quote | not_quote | eol ) ^^ { _.mkString }

  //QUOTE, fs, ls以外から開始し、fs, ls以外が後続する、長さ0以上の文字列。
  lazy val raw_value     = ( not_quote_and_delim ~ rep( not_delim )).? ^^ { case Some(head ~ tail) => head :: tail mkString; case None => "" }

  lazy val padding         = rep( space )
  lazy val escaped_quote   = quote ~ quote ^^^ quote
  lazy val not_quote       = not( quote ) ~> char
  lazy val not_delim       = not( delim ) ~> char
  lazy val not_quote_and_delim  = not( guard(quote) | delim ) ~> char

  lazy val char            = ".".r
  lazy val space           = ' '
  lazy val quote           = '"'
  lazy val delim           = '\t'
  lazy val eol: Parser[String] = "(\r\n|\r|\n)".r
}
