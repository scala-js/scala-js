import scala.scalajs.js.Dynamic.global

object ElementCreator {

  val $ = global.jQuery

  def create() = $("body").append($("<h1>Test</h1>"))

}
