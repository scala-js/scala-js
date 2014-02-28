/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation.{ JSName, JSExport }

@JSExport
object HelloWorld {
  @JSExport
  def main() {
    sayHelloFromDOM()
    sayHelloFromTypedDOM()
    sayHelloFromJQuery()
    sayHelloFromTypedJQuery()
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

  def alert(msg: js.String): Unit = ???
}

trait DOMDocument extends js.Object {
  def getElementById(id: js.String): DOMElement
  def createElement(tag: js.String): DOMElement
}

trait DOMElement extends js.Object {
  var innerHTML: js.String

  def appendChild(child: DOMElement): Unit
}

@JSName("jQuery")
object JQuery extends js.Object {
  def apply(selector: js.String): JQuery = ???
}

trait JQuery extends js.Object {
  def text(value: js.String): JQuery
  def text(): js.String

  def html(value: js.String): JQuery
  def html(): js.String

  def appendTo(parent: JQuery): JQuery
}
