
import com.github.rubyu.parsertuning._
import com.orangesignal.csv.CsvReader
import java.io.{File, FileInputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import scala.collection.JavaConversions._


object Main {

  def main(args: Array[String]) {

    val input = new InputStreamReader(new FileInputStream(new File(args(1))), StandardCharsets.UTF_8)
    val start = System.currentTimeMillis
    val list = args(0) match {
      case "OrangeSignal" =>
        import com.orangesignal.csv.CsvConfig
        val cfg = new CsvConfig('\t', '\"', '\"')
        cfg.setVariableColumns(true)
        val reader = new CsvReader(input, cfg)
        Iterator.continually(reader.readValues()).takeWhile(_ != null).map { row => row.toList }
      case "Parser1" =>
        new Reader(new Parser1, input)
          .collect { case e: Result.Row => e.value }
    }
    val result = list
      .map { row => print("."); row }
      .map { row => (1, row.size, row.map(_.size).sum) }
      .reduce { (a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3) }

    println("")

    val end = System.currentTimeMillis
    val totalSec = (end - start).toFloat / 1000
    println(s"total row: ${ result._1 }")
    println(s"total field: ${ result._2 }")
    println(s"total char: ${ result._3 }")
    println(s"total sec: ${ "%.4f" format totalSec }")
    println(s"row (per/sec): ${ "%.4f" format (result._1 / totalSec) }")
    println(s"field (per/sec): ${ "%.4f" format (result._2 / totalSec) }")
    println(s"char (per/sec): ${ "%.4f" format (result._3 / totalSec) }")
  }
}

