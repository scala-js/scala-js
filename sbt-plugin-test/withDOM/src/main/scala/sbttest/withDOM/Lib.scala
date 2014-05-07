package sbttest.withDOM

import scala.scalajs.js

object Lib {

  val document: js.Dynamic = js.Dynamic.global.document
  val jQuery: js.Dynamic = js.Dynamic.global.jQuery

  def getElementsByTagName(name: String): js.Array[js.Dynamic] =
    document.getElementsByTagName(name).asInstanceOf[js.Array[js.Dynamic]]

  /** appends a <p> with the message to the document */
  def appendDocument(msg: String): Unit = {
    val trg = {
      val bodies = getElementsByTagName("body")
      if (bodies.length > 0)
        bodies(0)
      else 
        document
    }

    val elem = document.createElement("p")
    elem.appendChild(document.createTextNode(msg))
    trg.appendChild(elem)
  }

}
