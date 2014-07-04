
package com.github.rubyu.parsertuning.wok

import org.specs2.mutable._
import org.specs2.specification.Scope


class WokParserTest extends SpecificationWithJUnit {


  "WokParser.ParserImpl with QuoteNone" should {
    "parse strings all but RS and FS" in {
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
  }

  "WokParser.ParserImpl with QuoteNone with Escape" should {
    "parse strings all but non-escaped RS and FS" in {
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
      parser.parse(parser.field, "a\\\t").get mustEqual "a\t"
      parser.parse(parser.field, "a\\\r\n").get mustEqual "a\r\n"
      parser.parse(parser.field, "a\\\r").get mustEqual "a\r"
      parser.parse(parser.field, "a\\\n").get mustEqual "a\n"
    }
    "parse RS and FS escaped with *" in {
      val FS = "\t".r
      val RS = "(\r\n|\r|\n)".r
      val FQ = Quote.None withEscape('*')
      val parser = new WokParser.ParserImpl(FS, RS, FQ)
      parser.parse(parser.field, "a*\t").get mustEqual "a\t"
      parser.parse(parser.field, "a*\r\n").get mustEqual "a\r\n"
      parser.parse(parser.field, "a*\r").get mustEqual "a\r"
      parser.parse(parser.field, "a*\n").get mustEqual "a\n"
    }
  }

}
