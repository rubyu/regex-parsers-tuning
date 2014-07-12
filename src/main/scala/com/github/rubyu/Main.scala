
import com.github.rubyu.parsertuning._
import com.orangesignal.csv.CsvReader
import java.io.{File, FileInputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import scala.collection.JavaConversions._
import wok.Quote
import wok.WokParser._


object Main {

  def gc() {
    print("GC ")
    System.gc()
    for (_ <- 0 until 5) {
      Thread.sleep(1000)
      print(".")
    }
    println("done.")
  }

  def main(args: Array[String]) {
    gc()

    val input = new InputStreamReader(new FileInputStream(new File(args(1))), StandardCharsets.UTF_8)
    val start = System.currentTimeMillis
    val itr = args(0) match {
      case "OrangeSignal" =>
        /* > sbt "run OrangeSignal test2.tsv"
         * total row: 12001
         * total field: 36001
         * total char: 47448571
         * total sec: 3.9770
         * row (per/sec): 3017.6013
         * field (per/sec): 9052.3008
         * char (per/sec): 11930745.0000
         * [success] Total time: 5 s, completed 2014/06/19 15:00:55
         * http://gyazo.com/569a5eba75a1555e222b7972bb819861
         */
        import com.orangesignal.csv.CsvConfig
        val cfg = new CsvConfig('\t', '\"', '\"')
        cfg.setVariableColumns(true)
        val reader = new CsvReader(input, cfg)
        Iterator.continually(reader.readValues()).takeWhile(_ != null).map { row => row.toList }
      case "Reader1WithParser1" =>
        /* > sbt "run Reader1WithParser1 test2.tsv"
         *
         * http://gyazo.com/0477f8b7bb6a02edbb8222f49380c6b4
         */
        new Reader1(new Parser1, input).collect { case e: Result.Row => e.value }
      case "Reader2WithParser1" =>
        /* > sbt "run Reader2WithParser1 test2.tsv"
         * total row: 12000
         * total field: 36000
         * total char: 47448571
         * total sec: 37.5700
         * row (per/sec): 319.4038
         * field (per/sec): 958.2114
         * char (per/sec): 1262937.7500
         * [success] Total time: 38 s, completed 2014/06/19 14:45:56
         * http://gyazo.com/fd15e0177dc00c1ddff28f139c9e5af3
         */
        new Reader2(new Parser1, input).collect { case e: Result.Row => e.value }
      case "Reader3WithParser1" =>
        /* > sbt "run Reader3WithParser1 test2.tsv"
         * total row: 12000
         * total field: 36000
         * total char: 47448571
         * total sec: 39.3310
         * row (per/sec): 305.1028
         * field (per/sec): 915.3085
         * char (per/sec): 1206391.1250
         * GC .....done.
         * [success] Total time: 50 s, completed 2014/06/19 19:14:53
         * http://gyazo.com/a536b99df12544feff81c18823e1262e
         */
        new Reader3(new Parser1, input).collect { case e: Result.Row => e.value }
      case "Reader3WithParser2" =>
        /* > sbt "run Reader3WithParser2 test2.tsv"
         * total row: 12000
         * total field: 36000
         * total char: 47448571
         * total sec: 3.3910
         * row (per/sec): 3538.7791
         * field (per/sec): 10616.3369
         * char (per/sec): 13992501.0000
         * GC .....done.
         * [success] Total time: 14 s, completed 2014/06/20 12:29:40
         * http://gyazo.com/c01c0eddc404381ec9813fd46994f97d
         */
        new Reader3(new Parser2, input).collect { case e: Result.Row => e.value }
      case "Reader3WithParser3" =>
        /* > sbt "run Reader3WithParser3 test2.tsv"
         * total row: 12000
         * total field: 36000
         * total char: 47448571
         * total sec: 3.3130
         * row (per/sec): 3622.0947
         * field (per/sec): 10866.2842
         * char (per/sec): 14321936.0000
         * GC .....done.
         * [success] Total time: 14 s, completed 2014/06/20 12:27:07
         * http://gyazo.com/729309975e866c310256cd5cd562f3e6
         */
        new Reader3(new Parser3, input).collect { case e: Result.Row => e.value }
      case "Reader4WithParser3" =>
        /* > sbt "run Reader4WithParser3 test2.tsv"
         * total row: 12000
         * total field: 36000
         * total char: 47448571
         * total sec: 4.5540
         * row (per/sec): 2635.0461
         * field (per/sec): 7905.1387
         * char (per/sec): 10419098.0000
         * GC .....done.
         * [success] Total time: 15 s, completed 2014/06/24 14:09:29
         */
        new Reader4(new Parser3, input).collect { case e: Result.Row => e.value }
      case "Wok" =>
        /* > sbt "run Wok test2.tsv"
         * total row: 12000
         * total field: 36000
         * total char: 47448571
         * total sec: 3.6510
         * row (per/sec): 3286.7708
         * field (per/sec): 9860.3125
         * char (per/sec): 12996048.0000
         * GC .....done.
         * [success] Total time: 25 s, completed 2014/07/13 8:27:51
         */
        val wok = new AbstractWok {
          val FS = "\t".r
          val RS = "(\r\n|\r|\n)".r
          val FQ = Quote.Min
          val parser = new ParserImpl(FS, RS, FQ)
        }
        val reader = new RowReader(input, wok)
        reader.collect { case e: Row => e.field }
    }

    val result = itr
      .map { row => print("."); row }
      .map { row => (1, row.size, row.map(_.size).sum) }
      .reduce { (a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3) }

    val end = System.currentTimeMillis
    val totalSec = (end - start).toFloat / 1000

    println("")
    println(s"total row: ${ result._1 }")
    println(s"total field: ${ result._2 }")
    println(s"total char: ${ result._3 }")
    println(s"total sec: ${ "%.4f" format totalSec }")
    println(s"row (per/sec): ${ "%.4f" format (result._1 / totalSec) }")
    println(s"field (per/sec): ${ "%.4f" format (result._2 / totalSec) }")
    println(s"char (per/sec): ${ "%.4f" format (result._3 / totalSec) }")

    gc()
  }
}

