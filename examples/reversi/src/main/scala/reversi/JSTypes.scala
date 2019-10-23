/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package reversi

import scala.scalajs.js

@js.native
trait Window extends js.Object {
  val document: DOMDocument = js.native

  def alert(msg: String): Unit = js.native
}

@js.native
trait DOMDocument extends js.Object {
  def getElementById(id: String): DOMElement = js.native
  def createElement(tag: String): DOMElement = js.native
}

@js.native
trait DOMElement extends js.Object {
  var innerHTML: String = js.native

  def appendChild(child: DOMElement): Unit = js.native
}

@js.native
trait JQueryStatic extends js.Object {
  def apply(arg: js.Any): JQuery = js.native
  def apply(arg: js.Any, attributes: js.Any): JQuery = js.native
}

@js.native
trait JQuery extends js.Object {
  def get(index: Int): DOMElement = js.native

  def text(value: String): JQuery = js.native
  def text(): String = js.native

  def html(value: String): JQuery = js.native
  def html(): String = js.native

  def prop(property: String): js.Any = js.native
  def prop(property: String, value: js.Any): JQuery = js.native

  def offset(): JQueryOffset = js.native

  def appendTo(parent: JQuery): JQuery = js.native
  def append(children: JQuery): JQuery = js.native

  def addClass(classes: String): JQuery = js.native
  def removeClass(classes: String): JQuery = js.native

  def each[U](callback: js.Function2[Int, js.Dynamic, U]): JQuery = js.native

  def click[U](handler: js.Function0[U]): JQuery = js.native
  def click[U](handler: js.Function1[JQueryEvent, U]): JQuery = js.native
}

@js.native
trait JQueryOffset extends js.Object {
  val top: Double = js.native
  val left: Double = js.native
}

@js.native
trait JQueryEvent extends js.Object {
  val pageX: Double = js.native
  val pageY: Double = js.native
}

@js.native
trait HTMLCanvasElement extends DOMElement {
  def getContext(kind: String): js.Any = js.native // depends on the kind
}

@js.native
trait CanvasRenderingContext2D extends js.Object {
  val canvas: HTMLCanvasElement = js.native

  var fillStyle: String = js.native
  var lineWidth: Double = js.native

  def fillRect(x: Double, y: Double, w: Double, h: Double): Unit = js.native
  def strokeRect(x: Double, y: Double, w: Double, h: Double): Unit = js.native

  def beginPath(): Unit = js.native
  def fill(): Unit = js.native
  def stroke(): Unit = js.native

  def arc(x: Double, y: Double, radius: Double, startAngle: Double,
      endAngle: Double, anticlockwise: Boolean): Unit = js.native
}
