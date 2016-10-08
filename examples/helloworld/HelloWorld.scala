/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation._

object HelloWorld extends js.JSApp {
  def main() {
    import js.DynamicImplicits.truthValue

    if (js.Dynamic.global.document &&
        js.Dynamic.global.document.getElementById("playground")) {
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

@js.native
@JSGlobalScope
object window extends js.Object {
  val document: DOMDocument = js.native

  def alert(msg: String): Unit = js.native
}

@js.native
trait DOMDocument extends js.Object {
  def getElementById(id: String): DOMElement = js.native
  def createElement(tag: String): DOMElement = js.native
}

@js.native
trait DOMElement extends js.Object {
  var innerHTML: String = js.native

  def appendChild(child: DOMElement): Unit = js.native
}

@js.native
@JSName("jQuery")
object JQuery extends js.Object {
  def apply(selector: String): JQuery = js.native
}

@js.native
trait JQuery extends js.Object {
  def text(value: String): JQuery = js.native
  def text(): String = js.native

  def html(value: String): JQuery = js.native
  def html(): String = js.native

  def appendTo(parent: JQuery): JQuery = js.native
}
