import scala.scalajs.js.Dynamic.global

object ElementCreator {
  val jQ = global.jQuery

  def create() = jQ("body").append(jQ("<h1>Test</h1>"))
}
