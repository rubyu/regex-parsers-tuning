
import com.github.rubyu.parsertuning._
import com.orangesignal.csv.CsvReader
import java.io.{File, FileInputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import scala.collection.JavaConversions._


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
         * http://gyazo.com/539ac30192c6d60fafc341db227bef18
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
         * http://gyazo.com/6941578699128973e6d1c4a7ff1d4ac8
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
total row: 12000
total field: 36000
total char: 47448571
total sec: 24.7570
row (per/sec): 484.7114
field (per/sec): 1454.1342
char (per/sec): 1916572.0000
GC .....done.
[success] Total time: 42 s, completed 2014/06/19 19:31:31
         */
        new Reader3(new Parser2, input).collect { case e: Result.Row => e.value }
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

