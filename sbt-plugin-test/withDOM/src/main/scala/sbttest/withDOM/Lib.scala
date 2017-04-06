package sbttest.withDOM

import scala.scalajs.js

object Lib {

  val document: js.Dynamic = js.Dynamic.global.document

  def getElementsByTagName(name: String): js.Array[js.Dynamic] =
    document.getElementsByTagName(name).asInstanceOf[js.Array[js.Dynamic]]

  /** appends a <p> with the message to the document */
  def appendDocument(msg: String): Unit = {
    val elem = document.createElement("p")
    elem.appendChild(document.createTextNode(msg))
    document.body.appendChild(elem)
  }

}
