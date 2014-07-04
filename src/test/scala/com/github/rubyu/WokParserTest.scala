
package com.github.rubyu.parsertuning.wok

import org.specs2.mutable._
import org.specs2.specification.Scope
import com.github.rubyu.parsertuning.wok.WokParser.Row1


class WokParserTest extends SpecificationWithJUnit {

  "WokParser.ParserImpl with QuoteNone" should {
    "parse non-quoted strings" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.None
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "").get mustEqual ""
      parser.parse(parser.field, "a").get mustEqual "a"
      parser.parse(parser.field, "a\t").get mustEqual "a"
      parser.parse(parser.field, "a\r\n").get mustEqual "a"
      parser.parse(parser.field, "a\r").get mustEqual "a"
      parser.parse(parser.field, "a\n").get mustEqual "a"
    }

    "parse Row" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.None
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "").get mustEqual Row1(Nil, Nil, "")
      parser.parse(parser.line, "a").get mustEqual Row1(List("a"), Nil, "")

      parser.parse(parser.line, "\r\n").get mustEqual Row1(Nil, Nil, "\r\n")
      parser.parse(parser.line, "\r").get mustEqual Row1(Nil, Nil, "\r")
      parser.parse(parser.line, "\n").get mustEqual Row1(Nil, Nil, "\n")

      parser.parse(parser.line, "a\r\n").get mustEqual Row1(List("a"), Nil, "\r\n")
      parser.parse(parser.line, "a\r").get mustEqual Row1(List("a"), Nil, "\r")
      parser.parse(parser.line, "a\n").get mustEqual Row1(List("a"), Nil, "\n")

      parser.parse(parser.line, "\t").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "a\t").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "a\tb").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "a\tb\tc").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")
    }
  }

  "WokParser.ParserImpl with QuoteNone with Escape" should {
    "parse non-quoted strings" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.None withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "").get mustEqual ""
      parser.parse(parser.field, "a").get mustEqual "a"
      parser.parse(parser.field, "a\t").get mustEqual "a"
      parser.parse(parser.field, "a\r\n").get mustEqual "a"
      parser.parse(parser.field, "a\r").get mustEqual "a"
      parser.parse(parser.field, "a\n").get mustEqual "a"
    }

    "parse non-quoted strings contain RS and FS escaped by \\" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.None withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\\\\").get mustEqual "\\"
      parser.parse(parser.field, "\\\t").get mustEqual "\t"
      parser.parse(parser.field, "\\\r\n").get mustEqual "\r\n"
      parser.parse(parser.field, "\\\r").get mustEqual "\r"
      parser.parse(parser.field, "\\\n").get mustEqual "\n"

      parser.parse(parser.field, "a\\\\").get mustEqual "a\\"
      parser.parse(parser.field, "a\\\t").get mustEqual "a\t"
      parser.parse(parser.field, "a\\\r\n").get mustEqual "a\r\n"
      parser.parse(parser.field, "a\\\r").get mustEqual "a\r"
      parser.parse(parser.field, "a\\\n").get mustEqual "a\n"
    }

    "parse non-quoted strings contain RS and FS escaped by *" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.None withEscape('*')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "**").get mustEqual "*"
      parser.parse(parser.field, "*\t").get mustEqual "\t"
      parser.parse(parser.field, "*\r\n").get mustEqual "\r\n"
      parser.parse(parser.field, "*\r").get mustEqual "\r"
      parser.parse(parser.field, "*\n").get mustEqual "\n"

      parser.parse(parser.field, "a**").get mustEqual "a*"
      parser.parse(parser.field, "a*\t").get mustEqual "a\t"
      parser.parse(parser.field, "a*\r\n").get mustEqual "a\r\n"
      parser.parse(parser.field, "a*\r").get mustEqual "a\r"
      parser.parse(parser.field, "a*\n").get mustEqual "a\n"
    }

    "not to parse non-quoted strings contain invalid escape expressions" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.None withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "\\").isEmpty must beTrue
      parser.parse(parser.line, "a\\").isEmpty must beTrue
      parser.parse(parser.line, "\\b").isEmpty must beTrue
    }

    "parse Row" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.None withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "").get mustEqual Row1(Nil, Nil, "")
      parser.parse(parser.line, "a").get mustEqual Row1(List("a"), Nil, "")

      parser.parse(parser.line, "\r\n").get mustEqual Row1(Nil, Nil, "\r\n")
      parser.parse(parser.line, "\r").get mustEqual Row1(Nil, Nil, "\r")
      parser.parse(parser.line, "\n").get mustEqual Row1(Nil, Nil, "\n")

      parser.parse(parser.line, "a\r\n").get mustEqual Row1(List("a"), Nil, "\r\n")
      parser.parse(parser.line, "a\r").get mustEqual Row1(List("a"), Nil, "\r")
      parser.parse(parser.line, "a\n").get mustEqual Row1(List("a"), Nil, "\n")

      parser.parse(parser.line, "\t").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "a\t").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "a\tb").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "a\tb\tc").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")

      parser.parse(parser.line, "\\\\").get mustEqual Row1(List("\\"), Nil, "")
      parser.parse(parser.line, "\\\t").get mustEqual Row1(List("\t"), Nil, "")
      parser.parse(parser.line, "\\\r\n").get mustEqual Row1(List("\r\n"), Nil, "")
      parser.parse(parser.line, "\\\r").get mustEqual Row1(List("\r"), Nil, "")
      parser.parse(parser.line, "\\\n").get mustEqual Row1(List("\n"), Nil, "")

      parser.parse(parser.line, "a\\\\").get mustEqual Row1(List("a\\"), Nil, "")
      parser.parse(parser.line, "a\\\t").get mustEqual Row1(List("a\t"), Nil, "")
      parser.parse(parser.line, "a\\\r\n").get mustEqual Row1(List("a\r\n"), Nil, "")
      parser.parse(parser.line, "a\\\r").get mustEqual Row1(List("a\r"), Nil, "")
      parser.parse(parser.line, "a\\\n").get mustEqual Row1(List("a\n"), Nil, "")

      parser.parse(parser.line, "a\\\\b").get mustEqual Row1(List("a\\b"), Nil, "")
      parser.parse(parser.line, "a\\\tb").get mustEqual Row1(List("a\tb"), Nil, "")
      parser.parse(parser.line, "a\\\r\nb").get mustEqual Row1(List("a\r\nb"), Nil, "")
      parser.parse(parser.line, "a\\\rb").get mustEqual Row1(List("a\rb"), Nil, "")
      parser.parse(parser.line, "a\\\nb").get mustEqual Row1(List("a\nb"), Nil, "")
    }
  }

  "WokParser.ParserImpl with QuoteMin" should {
    "parse non-quoted strings" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "").get mustEqual ""
      parser.parse(parser.field, "a").get mustEqual "a"
      parser.parse(parser.field, "a\t").get mustEqual "a"
      parser.parse(parser.field, "a\r\n").get mustEqual "a"
      parser.parse(parser.field, "a\r").get mustEqual "a"
      parser.parse(parser.field, "a\n").get mustEqual "a"
    }

    "not to parse non-quoted strings contain invalid quote expressions" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"").isEmpty must beTrue
      parser.parse(parser.field, "\"a").isEmpty must beTrue    //compatible with Python
      parser.parse(parser.field, "a\"").get mustEqual "a\""   //compatible with Python
      parser.parse(parser.field, "a\"b").get mustEqual "a\"b" //compatible with Python
    }

    "parse quoted strings" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"\"").get mustEqual ""
      parser.parse(parser.field, "\"a\"").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\t\"\"").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\r\n").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\r").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\n").get mustEqual "a"

      parser.parse(parser.field, "\"\"\"\"").get mustEqual "\""
    }

    "parse Row" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "").get mustEqual Row1(Nil, Nil, "")
      parser.parse(parser.line, "a").get mustEqual Row1(List("a"), Nil, "")

      parser.parse(parser.line, "\r\n").get mustEqual Row1(Nil, Nil, "\r\n")
      parser.parse(parser.line, "\r").get mustEqual Row1(Nil, Nil, "\r")
      parser.parse(parser.line, "\n").get mustEqual Row1(Nil, Nil, "\n")

      parser.parse(parser.line, "a\r\n").get mustEqual Row1(List("a"), Nil, "\r\n")
      parser.parse(parser.line, "a\r").get mustEqual Row1(List("a"), Nil, "\r")
      parser.parse(parser.line, "a\n").get mustEqual Row1(List("a"), Nil, "\n")

      parser.parse(parser.line, "\t").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "a\t").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "a\tb").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "a\tb\tc").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")


      parser.parse(parser.line, "\"\"").get mustEqual Row1(List(""), Nil, "")
      parser.parse(parser.line, "\"a\"").get mustEqual Row1(List("a"), Nil, "")

      parser.parse(parser.line, "\"\"\r\n").get mustEqual Row1(List(""), Nil, "\r\n")
      parser.parse(parser.line, "\"\"\r").get mustEqual Row1(List(""), Nil, "\r")
      parser.parse(parser.line, "\"\"\n").get mustEqual Row1(List(""), Nil, "\n")

      parser.parse(parser.line, "\"a\"\r\n").get mustEqual Row1(List("a"), Nil, "\r\n")
      parser.parse(parser.line, "\"a\"\r").get mustEqual Row1(List("a"), Nil, "\r")
      parser.parse(parser.line, "\"a\"\n").get mustEqual Row1(List("a"), Nil, "\n")

      parser.parse(parser.line, "\"\"\t\"\"").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"\"").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"b\"").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"b\"\t\"c\"").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")


      parser.parse(parser.line, "\t\"\"").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "a\t\"\"").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "a\t\"b\"").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "a\t\"b\"\t\"c\"").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")


      parser.parse(parser.line, "\"\"\t").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\tb").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "\"a\"\tb\t\"c\"").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")


      parser.parse(parser.line, "\"a\"\t\"b\"\tc").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")
    }
  }

  "WokParser.ParserImpl with QuoteMin with Escape" should {
    "parse non-quoted strings" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "").get mustEqual ""
      parser.parse(parser.field, "a").get mustEqual "a"
      parser.parse(parser.field, "a\t").get mustEqual "a"
      parser.parse(parser.field, "a\r\n").get mustEqual "a"
      parser.parse(parser.field, "a\r").get mustEqual "a"
      parser.parse(parser.field, "a\n").get mustEqual "a"
    }

    "parse non-quoted strings contain RS and FS escaped by \\" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\\\\").get mustEqual "\\"
      parser.parse(parser.field, "\\\t").get mustEqual "\t"
      parser.parse(parser.field, "\\\r\n").get mustEqual "\r\n"
      parser.parse(parser.field, "\\\r").get mustEqual "\r"
      parser.parse(parser.field, "\\\n").get mustEqual "\n"

      parser.parse(parser.field, "a\\\\").get mustEqual "a\\"
      parser.parse(parser.field, "a\\\t").get mustEqual "a\t"
      parser.parse(parser.field, "a\\\r\n").get mustEqual "a\r\n"
      parser.parse(parser.field, "a\\\r").get mustEqual "a\r"
      parser.parse(parser.field, "a\\\n").get mustEqual "a\n"
    }

    "parse non-quoted strings contain RS and FS escaped by *" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('*')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "**").get mustEqual "*"
      parser.parse(parser.field, "*\t").get mustEqual "\t"
      parser.parse(parser.field, "*\r\n").get mustEqual "\r\n"
      parser.parse(parser.field, "*\r").get mustEqual "\r"
      parser.parse(parser.field, "*\n").get mustEqual "\n"

      parser.parse(parser.field, "a**").get mustEqual "a*"
      parser.parse(parser.field, "a*\t").get mustEqual "a\t"
      parser.parse(parser.field, "a*\r\n").get mustEqual "a\r\n"
      parser.parse(parser.field, "a*\r").get mustEqual "a\r"
      parser.parse(parser.field, "a*\n").get mustEqual "a\n"
    }

    "not to parse non-quoted strings contain invalid quote expressions" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"").isEmpty must beTrue
      parser.parse(parser.field, "\"a").isEmpty must beTrue    //compatible with Python
      parser.parse(parser.field, "a\"").get mustEqual "a\""   //compatible with Python
      parser.parse(parser.field, "a\"b").get mustEqual "a\"b" //compatible with Python
    }

    "not to parse non-quoted strings contain invalid escape expressions" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "\\").isEmpty must beTrue
      parser.parse(parser.line, "a\\").isEmpty must beTrue
      parser.parse(parser.line, "\\b").isEmpty must beTrue
    }

    "parse quoted strings" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"\"").get mustEqual ""
      parser.parse(parser.field, "\"a\"").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\t\"\"").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\r\n").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\r").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\n").get mustEqual "a"

      parser.parse(parser.field, "\"\"\"\"").get mustEqual "\""
    }

    "parse quoted strings contain RS and FS escaped by \\" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"\\\"\"").get mustEqual "\""
      parser.parse(parser.field, "\"\\\\\"").get mustEqual "\\"
      parser.parse(parser.field, "\"\\\t\"").get mustEqual "\t"
      parser.parse(parser.field, "\"\\\r\n\"").get mustEqual "\r\n"
      parser.parse(parser.field, "\"\\\r\"").get mustEqual "\r"
      parser.parse(parser.field, "\"\\\n\"").get mustEqual "\n"

      parser.parse(parser.field, "\"a\\\"\"").get mustEqual "a\""
      parser.parse(parser.field, "\"a\\\\\"").get mustEqual "a\\"
      parser.parse(parser.field, "\"a\\\t\"").get mustEqual "a\t"
      parser.parse(parser.field, "\"a\\\r\n\"").get mustEqual "a\r\n"
      parser.parse(parser.field, "\"a\\\r\"").get mustEqual "a\r"
      parser.parse(parser.field, "\"a\\\n\"").get mustEqual "a\n"
    }

    "parse quoted strings contain RS and FS escaped by *" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('*')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"*\"\"").get mustEqual "\""
      parser.parse(parser.field, "\"**\"").get mustEqual "*"
      parser.parse(parser.field, "\"*\t\"").get mustEqual "\t"
      parser.parse(parser.field, "\"*\r\n\"").get mustEqual "\r\n"
      parser.parse(parser.field, "\"*\r\"").get mustEqual "\r"
      parser.parse(parser.field, "\"*\n\"").get mustEqual "\n"

      parser.parse(parser.field, "\"a*\"\"").get mustEqual "a\""
      parser.parse(parser.field, "\"a**\"").get mustEqual "a*"
      parser.parse(parser.field, "\"a*\t\"").get mustEqual "a\t"
      parser.parse(parser.field, "\"a*\r\n\"").get mustEqual "a\r\n"
      parser.parse(parser.field, "\"a*\r\"").get mustEqual "a\r"
      parser.parse(parser.field, "\"a*\n\"").get mustEqual "a\n"
    }

    "not to parse quoted strings contain invalid escape expressions" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.All withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "\"\"\"").isEmpty must beTrue
      parser.parse(parser.line, "\"\\\"").isEmpty must beTrue
      parser.parse(parser.line, "\"a\\\"").isEmpty must beTrue
      parser.parse(parser.line, "\"\\b\"").isEmpty must beTrue
    }

    "parse Row" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "").get mustEqual Row1(Nil, Nil, "")
      parser.parse(parser.line, "a").get mustEqual Row1(List("a"), Nil, "")

      parser.parse(parser.line, "\r\n").get mustEqual Row1(Nil, Nil, "\r\n")
      parser.parse(parser.line, "\r").get mustEqual Row1(Nil, Nil, "\r")
      parser.parse(parser.line, "\n").get mustEqual Row1(Nil, Nil, "\n")

      parser.parse(parser.line, "a\r\n").get mustEqual Row1(List("a"), Nil, "\r\n")
      parser.parse(parser.line, "a\r").get mustEqual Row1(List("a"), Nil, "\r")
      parser.parse(parser.line, "a\n").get mustEqual Row1(List("a"), Nil, "\n")

      parser.parse(parser.line, "\t").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "a\t").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "a\tb").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "a\tb\tc").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")


      parser.parse(parser.line, "\"\"").get mustEqual Row1(List(""), Nil, "")
      parser.parse(parser.line, "\"a\"").get mustEqual Row1(List("a"), Nil, "")

      parser.parse(parser.line, "\"\"\r\n").get mustEqual Row1(List(""), Nil, "\r\n")
      parser.parse(parser.line, "\"\"\r").get mustEqual Row1(List(""), Nil, "\r")
      parser.parse(parser.line, "\"\"\n").get mustEqual Row1(List(""), Nil, "\n")

      parser.parse(parser.line, "\"a\"\r\n").get mustEqual Row1(List("a"), Nil, "\r\n")
      parser.parse(parser.line, "\"a\"\r").get mustEqual Row1(List("a"), Nil, "\r")
      parser.parse(parser.line, "\"a\"\n").get mustEqual Row1(List("a"), Nil, "\n")

      parser.parse(parser.line, "\"\"\t\"\"").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"\"").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"b\"").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"b\"\t\"c\"").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")


      parser.parse(parser.line, "\t\"\"").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "a\t\"\"").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "a\t\"b\"").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "a\t\"b\"\t\"c\"").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")


      parser.parse(parser.line, "\"\"\t").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\tb").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "\"a\"\tb\t\"c\"").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")


      parser.parse(parser.line, "\"a\"\t\"b\"\tc").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")
    }
  }

  "WokParser.ParserImpl with QuoteAll" should {
    "parse quoted strings" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.All
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"\"").get mustEqual ""
      parser.parse(parser.field, "\"a\"").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\t\"\"").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\r\n").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\r").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\n").get mustEqual "a"

      parser.parse(parser.field, "\"\"\"\"").get mustEqual "\""
    }

    "parse Row" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.All
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "\"\"").get mustEqual Row1(List(""), Nil, "")
      parser.parse(parser.line, "\"a\"").get mustEqual Row1(List("a"), Nil, "")

      parser.parse(parser.line, "\"\"\r\n").get mustEqual Row1(List(""), Nil, "\r\n")
      parser.parse(parser.line, "\"\"\r").get mustEqual Row1(List(""), Nil, "\r")
      parser.parse(parser.line, "\"\"\n").get mustEqual Row1(List(""), Nil, "\n")

      parser.parse(parser.line, "\"a\"\r\n").get mustEqual Row1(List("a"), Nil, "\r\n")
      parser.parse(parser.line, "\"a\"\r").get mustEqual Row1(List("a"), Nil, "\r")
      parser.parse(parser.line, "\"a\"\n").get mustEqual Row1(List("a"), Nil, "\n")

      parser.parse(parser.line, "\"\"\t\"\"").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"\"").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"b\"").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"b\"\t\"c\"").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")
    }
  }

  "WokParser.ParserImpl with QuoteAll with Escape" should {
    "parse quoted strings" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.All withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"\"").get mustEqual ""
      parser.parse(parser.field, "\"a\"").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\t\"\"").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\r\n").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\r").get mustEqual "a"
      parser.parse(parser.field, "\"a\"\n").get mustEqual "a"

      parser.parse(parser.field, "\"\"\"\"").get mustEqual "\""
    }

    "parse quoted strings contain RS and FS escaped by \\" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"\\\"\"").get mustEqual "\""
      parser.parse(parser.field, "\"\\\\\"").get mustEqual "\\"
      parser.parse(parser.field, "\"\\\t\"").get mustEqual "\t"
      parser.parse(parser.field, "\"\\\r\n\"").get mustEqual "\r\n"
      parser.parse(parser.field, "\"\\\r\"").get mustEqual "\r"
      parser.parse(parser.field, "\"\\\n\"").get mustEqual "\n"

      parser.parse(parser.field, "\"a\\\"\"").get mustEqual "a\""
      parser.parse(parser.field, "\"a\\\\\"").get mustEqual "a\\"
      parser.parse(parser.field, "\"a\\\t\"").get mustEqual "a\t"
      parser.parse(parser.field, "\"a\\\r\n\"").get mustEqual "a\r\n"
      parser.parse(parser.field, "\"a\\\r\"").get mustEqual "a\r"
      parser.parse(parser.field, "\"a\\\n\"").get mustEqual "a\n"
    }

    "parse quoted strings contain RS and FS escaped by *" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.Min withEscape('*')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.field, "\"*\"\"").get mustEqual "\""
      parser.parse(parser.field, "\"**\"").get mustEqual "*"
      parser.parse(parser.field, "\"*\t\"").get mustEqual "\t"
      parser.parse(parser.field, "\"*\r\n\"").get mustEqual "\r\n"
      parser.parse(parser.field, "\"*\r\"").get mustEqual "\r"
      parser.parse(parser.field, "\"*\n\"").get mustEqual "\n"

      parser.parse(parser.field, "\"a*\"\"").get mustEqual "a\""
      parser.parse(parser.field, "\"a**\"").get mustEqual "a*"
      parser.parse(parser.field, "\"a*\t\"").get mustEqual "a\t"
      parser.parse(parser.field, "\"a*\r\n\"").get mustEqual "a\r\n"
      parser.parse(parser.field, "\"a*\r\"").get mustEqual "a\r"
      parser.parse(parser.field, "\"a*\n\"").get mustEqual "a\n"
    }

    "not to parse quoted strings contain invalid escape expressions" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.All withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "\"\"\"").isEmpty must beTrue
      parser.parse(parser.line, "\"\\\"").isEmpty must beTrue
      parser.parse(parser.line, "\"a\\\"").isEmpty must beTrue
      parser.parse(parser.line, "\"\\b\"").isEmpty must beTrue
    }

    "parse Row" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.All withEscape('\\')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)

      parser.parse(parser.line, "\"\"").get mustEqual Row1(List(""), Nil, "")
      parser.parse(parser.line, "\"a\"").get mustEqual Row1(List("a"), Nil, "")

      parser.parse(parser.line, "\"\"\r\n").get mustEqual Row1(List(""), Nil, "\r\n")
      parser.parse(parser.line, "\"\"\r").get mustEqual Row1(List(""), Nil, "\r")
      parser.parse(parser.line, "\"\"\n").get mustEqual Row1(List(""), Nil, "\n")

      parser.parse(parser.line, "\"a\"\r\n").get mustEqual Row1(List("a"), Nil, "\r\n")
      parser.parse(parser.line, "\"a\"\r").get mustEqual Row1(List("a"), Nil, "\r")
      parser.parse(parser.line, "\"a\"\n").get mustEqual Row1(List("a"), Nil, "\n")

      parser.parse(parser.line, "\"\"\t\"\"").get mustEqual Row1(List("", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"\"").get mustEqual Row1(List("a", ""), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"b\"").get mustEqual Row1(List("a", "b"), List("\t"), "")
      parser.parse(parser.line, "\"a\"\t\"b\"\t\"c\"").get mustEqual Row1(List("a", "b", "c"), List("\t", "\t"), "")
    }
  }
}
