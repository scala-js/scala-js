import scala.scalajs.js
import js.Dynamic.global

object ElementCreator {
  val jQ = global.jQuery

  def create(): js.Dynamic = jQ("body").append(jQ("<h1>Test</h1>"))
}
