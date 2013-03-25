package reversi

import scala.js._

trait Window extends JSObject {
  val document: DOMDocument

  def alert(msg: JSString): Unit
}

trait DOMDocument extends JSObject {
  def getElementById(id: JSString): DOMElement
  def createElement(tag: JSString): DOMElement
}

trait DOMElement extends JSObject {
  var innerHTML: JSString

  def appendChild(child: DOMElement): Unit
}

trait JQueryStatic extends JSObject {
  def apply(arg: JSAny): JQuery
  def apply(arg: JSAny, attributes: JSDictionary): JQuery
}

trait JQuery extends JSObject {
  def get(index: JSNumber): DOMElement

  def text(value: JSString): JQuery
  def text(): JSString

  def html(value: JSString): JQuery
  def html(): JSString

  def prop(property: JSString): JSAny
  def prop(property: JSString, value: JSAny): JQuery

  def offset(): JQueryOffset

  def appendTo(parent: JQuery): JQuery
  def append(children: JQuery): JQuery

  def addClass(classes: JSString): JQuery
  def removeClass(classes: JSString): JQuery

  def each(callback: JSFunction2[JSNumber, JSDynamic, JSAny]): JQuery

  def click(handler: JSFunction0[JSAny]): JQuery
  def click(handler: JSFunction1[JQueryEvent, JSAny]): JQuery
}

trait JQueryOffset extends JSObject {
  val top: JSNumber
  val left: JSNumber
}

trait JQueryEvent extends JSObject {
  val pageX: JSNumber
  val pageY: JSNumber
}

trait HTMLCanvasElement extends DOMElement {
  def getContext(kind: JSString): JSAny // depends on the kind
}

trait CanvasRenderingContext2D extends JSObject {
  val canvas: HTMLCanvasElement

  var fillStyle: JSString
  var lineWidth: JSNumber

  def fillRect(x: JSNumber, y: JSNumber, w: JSNumber, h: JSNumber)
  def strokeRect(x: JSNumber, y: JSNumber, w: JSNumber, h: JSNumber)

  def beginPath()
  def fill()
  def stroke()

  def arc(x: JSNumber, y: JSNumber, radius: JSNumber,
      startAngle: JSNumber, endAngle: JSNumber, anticlockwise: JSBoolean)
}
