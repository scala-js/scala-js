import scala.scalajs.js
import js.Dynamic.global

object ElementCreator {
  def create(): Unit = {
    val h1 = global.document.createElement("h1")
    h1.innerHTML = "Test"
    global.document.body.appendChild(h1)
  }
}
