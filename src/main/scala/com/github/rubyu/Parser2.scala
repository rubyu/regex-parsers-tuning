
package com.github.rubyu.parsertuning

class Parser2 extends Parser {

  lazy val fs : Parser[String] = "\t"
  lazy val ls : Parser[String] = "(\r\n|\r|\n)".r

  //長さ0以上の文字列。
  lazy val field        : Parser[String] = quoted_field | raw_value

  //QUOTEに囲まれていること。前後にスペースによるパディングが存在してもよい。
  lazy val quoted_field : Parser[String] = " *\"".r ~> quoted_value <~ "\" *".r

  //QUOTE以外の文字, エスケープされたQUOTEからなる長さ0以上の文字列。
  lazy val quoted_value : Parser[String] = rep( "\"\"" ^^^ "\"" | "[^\"]+".r ) ^^ { _.mkString }

  //QUOTE, fs, ls以外から開始し、fs, ls以外が後続する、長さ0以上の文字列。
  lazy val raw_value    : Parser[String] = "([^\t\"\r\n][^\t\r\n]*)?".r
}
