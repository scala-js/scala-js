package helloworld

import scala.scalajs.js
import org.scalajs.dom

object Main {
  def main(args: Array[String]): Unit = {
    val documentType = classOf[dom.Document].getName
    println(s"Document type: $documentType")
  }
}
