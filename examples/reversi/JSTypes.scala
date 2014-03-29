/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package reversi

import scala.scalajs.js

trait Window extends js.Object {
  val document: DOMDocument

  def alert(msg: String): Unit
}

trait DOMDocument extends js.Object {
  def getElementById(id: String): DOMElement
  def createElement(tag: String): DOMElement
}

trait DOMElement extends js.Object {
  var innerHTML: String

  def appendChild(child: DOMElement): Unit
}

trait JQueryStatic extends js.Object {
  def apply(arg: js.Any): JQuery
  def apply(arg: js.Any, attributes: js.Any): JQuery
}

trait JQuery extends js.Object {
  def get(index: Int): DOMElement

  def text(value: String): JQuery
  def text(): String

  def html(value: String): JQuery
  def html(): String

  def prop(property: String): js.Any
  def prop(property: String, value: js.Any): JQuery

  def offset(): JQueryOffset

  def appendTo(parent: JQuery): JQuery
  def append(children: JQuery): JQuery

  def addClass(classes: String): JQuery
  def removeClass(classes: String): JQuery

  def each[U](callback: js.Function2[Int, js.Dynamic, U]): JQuery

  def click[U](handler: js.Function0[U]): JQuery
  def click[U](handler: js.Function1[JQueryEvent, U]): JQuery
}

trait JQueryOffset extends js.Object {
  val top: Double
  val left: Double
}

trait JQueryEvent extends js.Object {
  val pageX: Double
  val pageY: Double
}

trait HTMLCanvasElement extends DOMElement {
  def getContext(kind: String): js.Any // depends on the kind
}

trait CanvasRenderingContext2D extends js.Object {
  val canvas: HTMLCanvasElement

  var fillStyle: String
  var lineWidth: Double

  def fillRect(x: Double, y: Double, w: Double, h: Double): Unit
  def strokeRect(x: Double, y: Double, w: Double, h: Double): Unit

  def beginPath(): Unit
  def fill(): Unit
  def stroke(): Unit

  def arc(x: Double, y: Double, radius: Double,
      startAngle: Double, endAngle: Double, anticlockwise: Boolean): Unit
}
