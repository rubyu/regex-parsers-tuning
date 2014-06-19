
package com.github.rubyu.parsertuning

class Parser2 extends Parser {

  lazy val row: Parser[List[String]] = repsep( field, delim )

  //長さ1以上
  lazy val field: Parser[String] = quoted_field | raw_value

  //QUOTEに囲まれていること。前後にスペースによるパディングが存在してもよい。
  lazy val quoted_field: Parser[String] = " *\"".r ~> quoted_value <~ "\" *".r

  //(QUOTE以外、ダブルクォート、改行)からなる長さ0以上の文字列。
  lazy val quoted_value: Parser[String] = rep( "\"\"" ^^^ "\"" | "[^\"]+".r ) ^^ { _.mkString }

  //QUOTE, DELIM以外から開始し、DELIM以外が後続する、長さ0以上の文字列。
  lazy val raw_value: Parser[String] = "([^\t\"\r\n][^\t\r\n]*)?".r

  lazy val delim           = '\t'
  lazy val eol: Parser[String] = "(\r\n|\r|\n)".r
}
