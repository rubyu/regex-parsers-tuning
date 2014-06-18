
package com.github.rubyu.parsertuning


object Result {
  trait Element
  case class Row(value: List[String]) extends Element
  case class EOL(value: String) extends Element
  case class InvalidString(value: String) extends Element
}
