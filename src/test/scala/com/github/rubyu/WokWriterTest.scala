
package com.github.rubyu.parsertuning.wok

import org.specs2.mutable._
import org.specs2.specification.Scope
import com.github.rubyu.parsertuning.wok.WokParser._
import java.io
import io.{PrintStream, ByteArrayOutputStream}


class WokWriterTest extends SpecificationWithJUnit {

  class scope extends Scope {
    val out = new ByteArrayOutputStream
    val pout = new PrintStream(out)
  }

  "Writer" should {

    "with Quote.All.E(\\)" in {
      "write a String" in new scope {
        new Writer(",", "\n", Quote.All.E('\\')).write(pout, "a")
        out.toString("utf-8") mustEqual "\"a\"\n"
      }
      "write a Seq" in new scope {
        new Writer(",", "\n", Quote.All.E('\\')).write(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "\"a\",\"b\"\n"
      }
      "write a String containing E" in new scope {
        new Writer(",", "\n", Quote.All.E('\\')).write(pout, "\\")
        out.toString("utf-8") mustEqual "\"\\\\\"\n"
      }
      "write a String containing Q" in new scope {
        new Writer(",", "\n", Quote.All.E('\\')).write(pout, "\"")
        out.toString("utf-8") mustEqual "\"\\\"\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer(",", "\n", Quote.All.E('\\')).write(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer(",", "\n", Quote.All.E('\\')).write(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with Quote.All" in {
      "write a String" in new scope {
        new Writer(",", "\n", Quote.All).write(pout, "a")
        out.toString("utf-8") mustEqual "\"a\"\n"
      }
      "write a Seq" in new scope {
        new Writer(",", "\n", Quote.All).write(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "\"a\",\"b\"\n"
      }
      "write a String containing Q" in new scope {
        new Writer(",", "\n", Quote.All).write(pout, "\"")
        out.toString("utf-8") mustEqual "\"\"\"\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer(",", "\n", Quote.All).write(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer(",", "\n", Quote.All).write(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with Quote.Min.E(\\)" in {
      "write a String" in new scope {
        new Writer(",", "\n", Quote.Min.E('\\')).write(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq" in new scope {
        new Writer(",", "\n", Quote.Min.E('\\')).write(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "write a String containing E" in new scope {
        new Writer(",", "\n", Quote.Min.E('\\')).write(pout, "\\")
        out.toString("utf-8") mustEqual "\\\\\n"
      }
      "write a String containing Q" in new scope {
        new Writer(",", "\n", Quote.Min.E('\\')).write(pout, "\"")
        out.toString("utf-8") mustEqual "\\\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer(",", "\n", Quote.Min.E('\\')).write(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer(",", "\n", Quote.Min.E('\\')).write(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with Quote.Min" in {
      "write a String" in new scope {
        new Writer(",", "\n", Quote.Min).write(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq" in new scope {
        new Writer(",", "\n", Quote.Min).write(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "write a String containing Q" in new scope {
        new Writer(",", "\n", Quote.Min).write(pout, "\"")
        out.toString("utf-8") mustEqual "\"\"\"\"\n"
      }
      "write a String containing OFS" in new scope {
        new Writer(",", "\n", Quote.Min).write(pout, ",")
        out.toString("utf-8") mustEqual "\",\"\n"
      }
      "write a String containing ORS" in new scope {
        new Writer(",", "\n", Quote.Min).write(pout, "\n")
        out.toString("utf-8") mustEqual "\"\n\"\n"
      }
    }

    "with Quote.None.E(\\)" in {
      "write a String" in new scope {
        new Writer(",", "\n", Quote.None.E('\\')).write(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq" in new scope {
        new Writer(",", "\n", Quote.None.E('\\')).write(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "write a String containing E" in new scope {
        new Writer(",", "\n", Quote.None.E('\\')).write(pout, "\\")
        out.toString("utf-8") mustEqual "\\\\\n"
      }
      "write a String containing OFS" in new scope {
        new Writer(",", "\n", Quote.None.E('\\')).write(pout, ",")
        out.toString("utf-8") mustEqual "\\,\n"
      }
      "throw an exception when a given string contains ORS" in new scope {
        new Writer(",", "\n", Quote.None.E('\\')).write(pout, "\n") must throwA[RuntimeException]
      }
    }

    "with Quote.None" in {
      "write a String with Quote.None" in new scope {
        new Writer(",", "\n", Quote.None).write(pout, "a")
        out.toString("utf-8") mustEqual "a\n"
      }
      "write a Seq with Quote.None" in new scope {
        new Writer(",", "\n", Quote.None).write(pout, Seq("a", "b"))
        out.toString("utf-8") mustEqual "a,b\n"
      }
      "throw an exception when a given string contains OFS with Quote.None" in new scope {
        new Writer(",", "\n", Quote.None).write(pout, ",") must throwA[RuntimeException]
      }
      "throw an exception when a given string contains ORS with Quote.None" in new scope {
        new Writer(",", "\n", Quote.None).write(pout, "\n") must throwA[RuntimeException]
      }
    }


  }
}