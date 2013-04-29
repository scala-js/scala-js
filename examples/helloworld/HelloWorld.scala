package helloworld

import scala.js._

object HelloWorld {
  def main(args: Array[String]) {
    sayHelloFromDOM()
    sayHelloFromTypedDOM()
    sayHelloFromJQuery()
    sayHelloFromTypedJQuery()
  }

  def sayHelloFromDOM() {
    val document = JSDynamic.global.document
    val playground = document.getElementById("playground")

    val newP = document.createElement("p")
    // This should work, but that seems to be a typer bug in updateDynamic()
    //newP.innerHTML = "Hello world! <i>-- DOM</i>"
    newP.updateDynamic("innerHTML")("Hello world! <i>-- DOM</i>")
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
    val jQuery = JSDynamic.global.jQuery
    val newP = jQuery("<p>").html("Hello world! <i>-- jQuery</i>")
    newP.appendTo(jQuery("#playground"))
  }

  def sayHelloFromTypedJQuery() {
    val jQuery = helloworld.jQuery
    val newP = jQuery("<p>").html("Hello world! <i>-- typed jQuery</i>")
    newP.appendTo(jQuery("#playground"))
  }
}

object window extends JSGlobalScope {
  val document: DOMDocument = ???

  def alert(msg: JSString): Unit = ???
}

trait DOMDocument extends JSObject {
  def getElementById(id: JSString): DOMElement
  def createElement(tag: JSString): DOMElement
}

trait DOMElement extends JSObject {
  var innerHTML: JSString

  def appendChild(child: DOMElement): Unit
}

object jQuery extends JSObject {
  def apply(selector: JSString): JQuery = ???
}

trait JQuery extends JSObject {
  def text(value: JSString): JQuery
  def text(): JSString

  def html(value: JSString): JQuery
  def html(): JSString

  def appendTo(parent: JQuery): JQuery
}
