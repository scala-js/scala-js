package helloworld

import scala.js.{ JavaScriptObject => JSO }

object HelloWorld extends App {
  sayHelloFromDOM()
  sayHelloFromJQuery()

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
}
