
package com.github.rubyu.parsertuning

class Parser2 extends Parser {

  lazy val row             = row_0 | row_1

  //サイズが2以上の列。フィールドが空になってもよい。
  //lazy val row_0           = field_0 ~ delim ~ rep1sep( field_0, delim ) ^^ { case head ~ _ ~ tail => head :: tail }
  lazy val row_0           = field_0 ~ delim ~ rep1sep( field_0, delim ) ^^ { case head ~ _ ~ tail => head :: tail }

  //サイズが1の列。フィールドが空になってはいけない。
  lazy val row_1           = field_1 ^^ { List(_) }

  //長さ0以上
  lazy val field_0: Parser[String] = quoted_field | raw_value_0
  //長さ1以上
  lazy val field_1: Parser[String] = quoted_field | raw_value_1

  //QUOTEに囲まれていること。前後にスペースによるパディングが存在してもよい。
  lazy val quoted_field: Parser[String] = " *\"".r ~> quoted_value <~ "\" *".r

  //(QUOTE以外、ダブルクォート、改行)からなる長さ0以上の文字列。
  lazy val quoted_value: Parser[String] = rep( "\"\"" ^^^ "\"" | "[^\"]+".r ) ^^ { _.mkString }

  //QUOTE, DELIM以外から開始し、DELIM以外が後続する、長さ0以上の文字列。
  //lazy val raw_value_0     = (not_quote_and_delim ~ rep( not_delim )).? ^^ { case Some(head ~ tail) => head :: tail mkString; case None => "" }
  lazy val raw_value_0: Parser[String] = "([^\t\"\r\n][^\t\r\n]*)?".r

  //QUOTE, DELIM以外から開始し、DELIM以外が後続する、長さ1以上の文字列。
  //lazy val raw_value_1     =  not_quote_and_delim ~ rep( not_delim ) ^^ { case head ~ tail => head :: tail mkString }
  lazy val raw_value_1: Parser[String] =  "[^\t\"\r\n][^\t\r\n]*".r

  //lazy val padding         = rep( space )
  //lazy val padding: Parser[String] = " *".r

  //lazy val escaped_quote   = quote ~ quote ^^^ quote
  //lazy val escaped_quote: Parser[String] = "\"\"" ^^^ "\""

  //lazy val not_quote       = not( quote ) ~> char
  //lazy val not_quote: Parser[String] = "[^\"\r\n]".r

  //lazy val not_delim       = not( delim ) ~> char
  //lazy val not_delim: Parser[String] = "[^\t\r\n]".r

  //lazy val not_quote_and_delim  = not( guard(quote) | delim ) ~> char
  //lazy val not_quote_and_delim: Parser[String] = "[^\t\"\r\n]".r

  //lazy val char            = ".".r
  //lazy val space           = ' '
  //lazy val quote           = '"'
  lazy val delim           = '\t'
  lazy val eol: Parser[String] = "[\r\n]+".r
}
