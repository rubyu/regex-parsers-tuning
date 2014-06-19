
package com.github.rubyu.parsertuning

import org.specs2.mutable._
import org.specs2.specification.Scope


class Parser1Test extends SpecificationWithJUnit {

  trait scope extends Scope {
    val parser = new Parser1
  }

  "Parser1.raw_value" should {
    "return empty string value when input is empty" in new scope {
      parser.parse(parser.raw_value, "")
        .get mustEqual ""
    }

    "parse a character" in new scope {
      parser.parse(parser.raw_value, "a")
        .get mustEqual "a"
    }

    "parse characters" in new scope {
      parser.parse(parser.raw_value, "abc")
        .get mustEqual "abc"
    }

    "return empty string value when input is \\n" in new scope {
      parser.parse(parser.raw_value, "\n")
        .get mustEqual ""
    }

    "return empty string value when input is \\r" in new scope {
      parser.parse(parser.raw_value, "\r")
        .get mustEqual ""
    }

    "return empty string value when input is \\r\\n" in new scope {
      parser.parse(parser.raw_value, "\r\n")
        .get mustEqual ""
    }

    "parse until \\n" in new scope {
      parser.parse(parser.raw_value, "a\nb")
        .get mustEqual "a"
    }

    "parse until \\r" in new scope {
      parser.parse(parser.raw_value, "a\rb")
        .get mustEqual "a"
    }

    "parse until \\r\\n" in new scope {
      parser.parse(parser.raw_value, "a\r\nb")
        .get mustEqual "a"
    }
  }

  "Parser1.quoted_value" should {
    "parse a character" in new scope {
      parser.parse(parser.quoted_value, "a")
        .get mustEqual "a"
    }

    "parse characters" in new scope {
      parser.parse(parser.quoted_value, "abc")
        .get mustEqual "abc"
    }

    "parse a double quote to a single quote" in new scope {
      parser.parse(parser.quoted_value, "\"\"")
        .get mustEqual "\""
    }

    "parse characters contains a double quote" in new scope {
      parser.parse(parser.quoted_value, "a\"\"b")
        .get mustEqual "a\"b"
    }

    "return empty string when input starts with \"" in new scope {
      parser.parse(parser.quoted_value, "\"")
        .get mustEqual ""
    }

    "allow \\n" in new scope {
      parser.parse(parser.quoted_value, "\n")
        .get mustEqual "\n"
    }

    "allow \\r\\n" in new scope {
      parser.parse(parser.quoted_value, "\r\n")
        .get mustEqual "\r\n"
    }

    "parse input contains \\n" in new scope {
      parser.parse(parser.quoted_value, "a\nb")
        .get mustEqual "a\nb"
    }

    "parse input contains \\r\\n" in new scope {
      parser.parse(parser.quoted_value, "a\r\nb")
        .get mustEqual "a\r\nb"
    }
  }


  "Parser1.quoted_field" should {
    "return empty result when input does not starts with quote character" in new scope {
      parser.parse(parser.quoted_field, "a")
        .isEmpty must beTrue
    }

    "return empty result when input starts with quote character and breaks in the middle" in new scope {
      parser.parse(parser.quoted_field, "\"a")
        .isEmpty must beTrue
    }

    "parse input starts and ends with quote character" in new scope {
      parser.parse(parser.quoted_field, "\"a\"")
        .get mustEqual "a"
    }

    "parse input starts and ends with quote character until second quote character" in new scope {
      parser.parse(parser.quoted_field, "\"a\"b")
        .get mustEqual "a"
    }
  }

  "Parser1.field" should {
    "parse quoted text" in new scope {
      parser.parse(parser.field, "\"a\"")
        .get mustEqual "a"
    }

    "parse quoted text with padding before it" in new scope {
      parser.parse(parser.field, " \"a\"")
        .get mustEqual "a"
    }

    "parse quoted text with padding after it" in new scope {
      parser.parse(parser.field, "\"a\" ")
        .get mustEqual "a"
    }

    "parse quoted text with paddings both before and after" in new scope {
      parser.parse(parser.field, " \"a\" ")
        .get mustEqual "a"
    }

    "parse raw-text" in new scope {
      parser.parse(parser.field, "a")
        .get mustEqual "a"
    }

    "parse raw-text with padding before it as raw-text" in new scope {
      parser.parse(parser.field, " a")
        .get mustEqual " a"
    }

    "parse raw-text with padding after it as raw-text" in new scope {
      parser.parse(parser.field, "a ")
        .get mustEqual "a "
    }

    "parse raw-text with paddings both before and after it as raw-text" in new scope {
      parser.parse(parser.field, " a ")
        .get mustEqual " a "
    }

    "return empty string when the first character is QUOTE" in new scope {
      parser.parse(parser.field, "\"")
        .get mustEqual ""
    }

    "parse text ends with single quote as raw-text" in new scope {
      parser.parse(parser.field, "a\"")
        .get mustEqual "a\""
    }

    "parse text contains single quote as raw-text" in new scope {
      parser.parse(parser.field, "a\"b")
        .get mustEqual "a\"b"
    }

    "return empty string when input starts with \\n" in new scope {
      parser.parse(parser.field, "\na")
        .get mustEqual ""
    }

    "return empty string when input starts with \\r\\n" in new scope {
      parser.parse(parser.field, "\r\na")
        .get mustEqual ""
    }

    "parse raw-text until \\n" in new scope {
      parser.parse(parser.field, "a\nb")
        .get mustEqual "a"
    }

    "parse raw-text until \\r\\n" in new scope {
      parser.parse(parser.field, "a\r\nb")
        .get mustEqual "a"
    }

    "parse quoted-text contains \n" in new scope {
      parser.parse(parser.field, "\"a\nb\"")
        .get mustEqual "a\nb"
    }

    "parse quoted-text contains \r\n" in new scope {
      parser.parse(parser.field, "\"a\r\nb\"")
        .get mustEqual "a\r\nb"
    }
  }

  "Parser1.row" should {
    "return a list contains a emptry string when input is empty" in new scope {
      parser.parse(parser.row, "")
        .get mustEqual List("")
    }

    "return a list size is 1 when a raw-text is given" in new scope {
      parser.parse(parser.row, "a")
        .get mustEqual List("a")
    }

    "return a list size is 2 when tab separeted 2 raw-text are given" in new scope {
      parser.parse(parser.row, "a\tb")
        .get mustEqual List("a", "b")
    }

    "return a list size is 3 when tab separeted 3 raw-text are given" in new scope {
      parser.parse(parser.row, "a\tb\tc")
        .get mustEqual List("a", "b", "c")
    }

    "return a list size is 1 when a quoted-text is given" in new scope {
      parser.parse(parser.row, "\"a\"")
        .get mustEqual List("a")
    }

    "return a list size is 2 when tab separeted 2 quoted-text is given" in new scope {
      parser.parse(parser.row, "\"a\"\t\"b\"")
        .get mustEqual List("a", "b")
    }

    "return a list size is 2 when a tab is given" in new scope {
      parser.parse(parser.row, "\t")
        .get mustEqual List("", "")
    }

    "return a list size is 3 when 2 tabs are given" in new scope {
      parser.parse(parser.row, "\t\t")
        .get mustEqual List("", "", "")
    }

    "parse until the end of a row expression" in new scope {
      parser.parse(parser.row, "\"a\"b")
        .get mustEqual List("a")
      parser.parse(parser.row, "\"a\"b")
        .next.offset mustEqual 3
    }
  }

  "Parser1.lastLine" should {
    "return Row when row expression is given" in new scope {
      parser.parse(parser.lastLine, "a")
        .get mustEqual Result.Row(List("a"))
    }
  }

  "Parser1.line" should {
    "return Row when row expression is given and ends with \\n" in new scope {
      parser.parse(parser.line, "a\n")
        .get mustEqual Result.Row(List("a"))
    }

    "return Row when row expression is given and ends with \\r" in new scope {
      parser.parse(parser.line, "a\r")
        .get mustEqual Result.Row(List("a"))
    }

    "return Row when row expression is given and ends with \\r\\n" in new scope {
      parser.parse(parser.line, "a\r\n")
        .get mustEqual Result.Row(List("a"))
    }
  }
}
