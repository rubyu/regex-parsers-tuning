
package com.github.rubyu.parsertuning.wok

import org.specs2.mutable._
import org.specs2.specification.Scope
import com.github.rubyu.parsertuning.wok.WokParser._
import java.io


class RowReaderTest extends SpecificationWithJUnit {

  "RowReader" should {
    "return Row" in {
      val wok = new AbstractWok {
        val FS = "\t".r
        val RS = "(\r\n|\r|\n)".r
        val FQ = Quote.None
        def parser = new ParserImpl(FS, RS, FQ)
      }
      val reader = new RowReader(new io.StringReader("a1\ta2\ta3\nb1\tb2\tb3"), wok)
      reader.toList mustEqual List(
        Row(0, List("a1", "a2", "a3"), List("\t", "\t"), "\n"),
        Row(1, List("b1", "b2", "b3"), List("\t", "\t"), ""))
    }
  }
}
