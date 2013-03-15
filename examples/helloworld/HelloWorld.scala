package helloworld

import scala.js._

object HelloWorld extends App {
  sayHelloFromDOM()
  sayHelloFromTypedDOM()
  sayHelloFromJQuery()
  sayHelloFromTypedJQuery()

  def sayHelloFromDOM() {
    val document = JSDynamic.window.document
    val playground = document.getElementById("playground")

    val newP = document.createElement("p")
    // This should work, but that seems to be a typer bug in updateDynamic()
    //newP.innerHTML = "Hello world! <i>-- DOM</i>"
    newP.updateDynamic("innerHTML")("Hello world! <i>-- DOM</i>")
    playground.appendChild(newP)
  }

  def sayHelloFromTypedDOM() {
    val window = JSDynamic.window.asInstanceOf[JSWindow]
    val document = window.document
    val playground = document.getElementById("playground")

    val newP = document.createElement("p")
    newP.innerHTML = "Hello world! <i>-- typed DOM</i>"
    playground.appendChild(newP)
  }

  def sayHelloFromJQuery() {
    // val $ is fine too, but not very recommended in Scala code
    val jQuery = JSDynamic.window.$
    val playground = jQuery("#playground")
    jQuery("<p>").html("Hello world! <i>-- jQuery</i>").appendTo(playground);
  }

  def sayHelloFromTypedJQuery() {
    val window = JSDynamic.window.asInstanceOf[JSWindow]
    val jQuery = window.$
    val newP = jQuery("<p>").html("Hello world! <i>-- typed jQuery</i>")
    newP.appendTo(jQuery("#playground"))
  }

  abstract class JSWindow extends JSObject {
    val document: DOMDocument

    def alert(msg: JSString): Unit

    val jQuery: JSJQueryStatic
    val $: JSJQueryStatic
  }

  trait DOMDocument extends JSObject {
    def getElementById(id: JSString): DOMElement
    def createElement(tag: JSString): DOMElement
  }

  trait DOMElement extends JSObject {
    var innerHTML: JSString

    def appendChild(child: DOMElement): Unit
  }

  trait JSJQueryStatic extends JSObject {
    def apply(selector: JSString): JSJQuery
  }

  trait JSJQuery extends JSObject {
    def text(value: JSString): JSJQuery
    def text(): JSString

    def html(value: JSString): JSJQuery
    def html(): JSString

    def appendTo(parent: JSJQuery): JSJQuery
  }
}
