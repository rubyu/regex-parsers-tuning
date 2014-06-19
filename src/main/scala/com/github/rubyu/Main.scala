
import com.github.rubyu.parsertuning._
import com.orangesignal.csv.CsvReader
import java.io.{File, FileInputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import scala.collection.JavaConversions._


object Main {

  def main(args: Array[String]) {

    val input = new InputStreamReader(new FileInputStream(new File(args(1))), StandardCharsets.UTF_8)
    val start = System.currentTimeMillis
    val itr = args(0) match {
      case "OrangeSignal" =>
        /* > sbt "run OrangeSignal test2.tsv"
         * total row: 12001
         * total field: 36001
         * total char: 47448571
         * total sec: 2.7690
         * row (per/sec): 4334.0557
         * field (per/sec): 13001.4443
         * char (per/sec): 17135634.0000
         * [success] Total time: 11 s, completed 2014/06/18 22:05:35
         */
        import com.orangesignal.csv.CsvConfig
        val cfg = new CsvConfig('\t', '\"', '\"')
        cfg.setVariableColumns(true)
        val reader = new CsvReader(input, cfg)
        Iterator.continually(reader.readValues()).takeWhile(_ != null).map { row => row.toList }
      case "Reader1WithParser1" =>
        /* > sbt "run Reader1WithParser1 test2.tsv"
         */
        new Reader1(new Parser1, input).collect { case e: Result.Row => e.value }
      case "Reader2WithParser1" =>
        /* > sbt "run Reader2WithParser1 test2.tsv"
         * total row: 12000
         * total field: 36000
         * total char: 47448571
         * total sec: 37.2180
         * row (per/sec): 322.4247
         * field (per/sec): 967.2739
         * char (per/sec): 1274882.3750
         * [success] Total time: 44 s, completed 2014/06/18 22:03:37
         */
        new Reader2(new Parser1, input).collect { case e: Result.Row => e.value }
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
  }
}

