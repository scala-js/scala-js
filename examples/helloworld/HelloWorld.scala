/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation.{ JSName, JSExport }

object HelloWorld extends js.JSApp {
  def main() {
    if (!(!js.Dynamic.global.document)) {
      sayHelloFromDOM()
      sayHelloFromTypedDOM()
      sayHelloFromJQuery()
      sayHelloFromTypedJQuery()
    } else {
      println("Hello world!")
    }
  }

  def sayHelloFromDOM() {
    val document = js.Dynamic.global.document
    val playground = document.getElementById("playground")

    val newP = document.createElement("p")
    newP.innerHTML = "Hello world! <i>-- DOM</i>"
    playground.appendChild(newP)
  }

  def sayHelloFromTypedDOM() {
    val document = window.document
    val playground = document.getElementById("playground")

    val newP = document.createElement("p")
    newP.innerHTML = "Hello world! <i>-- typed DOM</i>"
    playground.appendChild(newP)
  }

  def sayHelloFromJQuery() {
    // val $ is fine too, but not very recommended in Scala code
    val jQuery = js.Dynamic.global.jQuery
    val newP = jQuery("<p>").html("Hello world! <i>-- jQuery</i>")
    newP.appendTo(jQuery("#playground"))
  }

  def sayHelloFromTypedJQuery() {
    val jQuery = helloworld.JQuery
    val newP = jQuery("<p>").html("Hello world! <i>-- typed jQuery</i>")
    newP.appendTo(jQuery("#playground"))
  }
}

object window extends js.GlobalScope {
  val document: DOMDocument = ???

  def alert(msg: String): Unit = ???
}

trait DOMDocument extends js.Object {
  def getElementById(id: String): DOMElement
  def createElement(tag: String): DOMElement
}

trait DOMElement extends js.Object {
  var innerHTML: String

  def appendChild(child: DOMElement): Unit
}

@JSName("jQuery")
object JQuery extends js.Object {
  def apply(selector: String): JQuery = ???
}

trait JQuery extends js.Object {
  def text(value: String): JQuery
  def text(): String

  def html(value: String): JQuery
  def html(): String

  def appendTo(parent: JQuery): JQuery
}
