
package com.github.rubyu.parsertuning

import org.specs2.mutable._
import java.io.StringReader
import org.specs2.specification.Scope


class ReaderTest extends SpecificationWithJUnit {

  "Reader" should {

    trait scope extends Scope {
      val parsers = List(new Parser1)
    }

    "parse raw-text" in new scope {
      parsers map { p =>
        new Reader(p, new StringReader("a"))
          .toList mustEqual List(Result.Row(List("a")))
      }
    }

    "parse quoted-text" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("\"a\""))
          .toList mustEqual List(Result.Row(List("a")))
      }
    }

    "parse \\n" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("\n"))
          .toList mustEqual List()
      }
    }

    "parse \\r\\r\\n" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("\r\r\n"))
          .toList mustEqual List()
      }
    }

    "parse \\r\\n" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("\r\n"))
          .toList mustEqual List()
      }
    }

    "parse quoted-text contains \\n" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("\"a\nb\""))
          .toList mustEqual List(Result.Row(List("a\nb")))
      }
    }

    "parse raw-text, DELIM, raw-text" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("a\tb"))
          .toList mustEqual List(Result.Row(List("a", "b")))
      }
    }

    "parse raw-text, DELIM, raw-test, DELIM, raw-text" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("a\tb\tc"))
          .toList mustEqual List(Result.Row(List("a", "b", "c")))
      }
    }

    "parse raw-text, \\n, raw-text" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("a\nb"))
          .toList mustEqual List(Result.Row(List("a")), Result.Row(List("b")))
      }
    }

    "parse raw-text, \\n, raw-text, \\n, raw-text" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("a\nb\nc"))
          .toList mustEqual List(Result.Row(List("a")), Result.Row(List("b")), Result.Row(List("c")))
      }
    }

    "parse raw-text, \\n" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("a\n"))
          .toList mustEqual List(Result.Row(List("a")))
      }
    }

    "parse raw-text, \\r\\n" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("a\r\n"))
          .toList mustEqual List(Result.Row(List("a")))
      }
    }

    "parse raw-text, \\r\\r\\n" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("a\r\r\n"))
          .toList mustEqual List(Result.Row(List("a")))
      }
    }

    "parse un-closed quote-text as InvalidString" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("\"a"))
          .toList mustEqual List(Result.InvalidString("\"a"))
      }
    }

    "parse a Result.Row that has un-closed quote-text as Result.Row, InvalidString" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader("a\t\"b"))
          .toList mustEqual List(Result.Row(List("a", "")), Result.InvalidString("\"b"))
      }
    }

    "return empty input when empty input is given" in new scope {
      parsers map { p =>
        new Reader(new Parser1, new StringReader(""))
          .toList mustEqual List()
      }
    }
  }
}
