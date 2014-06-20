
package com.github.rubyu.parsertuning


class Parser1 extends Parser {

  lazy val fs : Parser[String] = "\t"
  lazy val ls : Parser[String] = "(\r\n|\r|\n)".r

  //長さ0以上の文字列。
  lazy val field             = quoted_field | raw_value

  //QUOTEに囲まれていること。前後にスペースによるパディングが存在してもよい。
  lazy val quoted_field      = padding ~> quote ~> quoted_value <~ quote <~ padding

  //QUOTE以外の文字, エスケープされたQUOTEからなる長さ0以上の文字列。
  lazy val quoted_value      = rep( escaped_quote | not_quote | ls | fs ) ^^ { _.mkString }

  //QUOTE, fs, ls以外から開始し、fs, ls以外が後続する、長さ0以上の文字列。
  lazy val raw_value         = ( not_quote_and_fs ~ rep( not_fs )).? ^^ { case Some(head ~ tail) => head :: tail mkString; case None => "" }

  lazy val padding           = rep( space )
  lazy val escaped_quote     = quote ~ quote ^^^ quote
  lazy val not_quote         = not( quote ) ~> char
  lazy val not_fs            = not( fs ) ~> char
  lazy val not_quote_and_fs  = not( guard(quote) | fs ) ~> char

  lazy val char            = ".".r
  lazy val space           = ' '
  lazy val quote           = '"'
}
