
package com.github.rubyu.parsertuning

import util.matching.Regex

class Parser3 extends Parser {

  val LS = "(\r\n|\r|\n)"
  val FS = "\t"

  val ls : Parser[String] = new Regex(LS)
  val fs : Parser[String] = new Regex(FS)

  //長さ0以上の文字列。
  lazy val field        : Parser[String] = quoted_field | raw_field

  //QUOTEに囲まれていること。前後にスペースによるパディングが存在してもよい。
  lazy val quoted_field : Parser[String] = " *\"".r ~> quoted_value <~ "\" *".r

  //QUOTE以外の文字, エスケープされたQUOTEからなる長さ0以上の文字列。
  lazy val quoted_value : Parser[String] = rep( "\"\"" ^^^ "\"" | """[^"]+""".r ) ^^ { _.mkString }

  //QUOTE, fs, ls以外から開始し、fs, ls以外が後続する、長さ0以上の文字列。
  lazy val raw_field    : Parser[String] = s"""(((?!$FS)(?!$LS)[^"])((?!$FS)(?!$LS).)*)?""".r
}
