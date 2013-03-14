/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.js

import scalajs._

/** Extension of ScalaPrimitives for primitives only relevant to the JS backend
 */
abstract class JSPrimitives {
  val global: JSGlobal

  import global._
  import jsDefinitions._
  import scalaPrimitives._

  val V2JS = 300
  val Z2JS = 301
  //val C2JS = 302
  val N2JS = 303
  val S2JS = 304

  val JS2Z = 311
  //val JS2C = 312
  val JS2N = 313
  val JS2S = 314

  val ANY2DYN = 320
  val WINDOW = 321
  val EMPTY_OBJ = 322

  def init() {
    if (!isScalaJSDefined)
      return

    addPrimitive(JSAny_fromUnit, V2JS)
    addPrimitive(JSAny_fromBoolean, Z2JS)
    addPrimitive(JSAny_fromByte, N2JS)
    addPrimitive(JSAny_fromShort, N2JS)
    addPrimitive(JSAny_fromInt, N2JS)
    addPrimitive(JSAny_fromLong, N2JS)
    addPrimitive(JSAny_fromFloat, N2JS)
    addPrimitive(JSAny_fromDouble, N2JS)
    addPrimitive(JSAny_fromString, S2JS)

    addPrimitive(JSBoolean_toBoolean, JS2Z)
    addPrimitive(JSNumber_toDouble, JS2N)
    addPrimitive(JSString_toScalaString, JS2S)

    addPrimitive(JSDynamic_fromAny, ANY2DYN)
    addPrimitive(JSDynamic_window, WINDOW)
    addPrimitive(JSObject_newEmpty, EMPTY_OBJ)
  }

  def isJavaScriptPrimitive(code: Int) =
    code >= 300 && code < 330
}
