package helloworld

import scala.js.{ JavaScriptObject => JSO, _ }

object HelloWorld extends App {
  sayHelloFromDOM()
  sayHelloFromJQuery()
  sayHelloFromTypedJQuery()

  def sayHelloFromDOM() {
    val document = JSO.window.document
    val playground = document.getElementById("playground")

    val newP = document.createElement("p")
    // This should work, but that seems to be a typer bug in updateDynamic()
    //newP.innerHTML = "Hello world! <i>-- DOM</i>"
    newP.updateDynamic("innerHTML")("Hello world! <i>-- DOM</i>")
    playground.appendChild(newP)
  }

  def sayHelloFromJQuery() {
    // val $ is fine too, but not very recommended in Scala code
    val jQuery = JSO.window.$
    val playground = jQuery("#playground")
    jQuery("<p>", Map(
        "html" -> "Hello world! <i>-- jQuery</i>"
    )).appendTo(playground);
  }

  abstract class JSWindow extends JSObject {
    val jQuery: JSJQueryStatic
    val $: JSJQueryStatic

    def alert(msg: JSString): Unit
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

  def sayHelloFromTypedJQuery(): String = {
    val window = JSDynamic.window.asInstanceOf[JSWindow]
    val jQuery = window.$
    val newP = jQuery("<p>").html("Hello world! <i>-- typed jQuery</i>")
    newP.appendTo(jQuery("#playground"))
    newP.text()
  }
}
