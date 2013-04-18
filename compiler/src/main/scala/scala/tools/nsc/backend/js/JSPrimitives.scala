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

  // Conversions from Scala types to JS types
  val V2JS = 300 // Unit
  val Z2JS = 301 // Boolean
  //val C2JS = 302 // Char
  val N2JS = 303 // Number (any numeric type)
  val S2JS = 304 // String
  val F2JS = 305 // FunctionN

  // Conversions from JS types to Scala types
  val JS2Z = 311 // Boolean
  //val JS2C = 312 // Char
  val JS2N = 313 // Number (any numeric type)
  val JS2S = 314 // String

  val ANY2DYN = 320 // Conversion from JSAny to JSDynamic
  val WINDOW = 321  // Get the top-level object (window)

  val DYNSELECT = 330 // JSDynamic.selectDynamic
  val DYNUPDATE = 331 // JSDynamic.updateDynamic
  val DYNAPPLY = 332  // JSDynamic.applyDynamic

  val DICT_SELECT = 333 // JSDictionary.apply
  val DICT_UPDATE = 334 // JSDictionary.update

  val ARR_CREATE = 335      // JSArray.create (array literal syntax)
  val ARR_GET = DICT_SELECT // JSArray.apply
  val ARR_SET = DICT_UPDATE // JSArray.update

  /** Initialize the map of primitive methods */
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

    addPrimitive(JSAny_fromFunction0, F2JS)
    addPrimitive(JSAny_fromFunction1, F2JS)
    addPrimitive(JSAny_fromFunction2, F2JS)

    addPrimitive(JSBoolean_toBoolean, JS2Z)
    addPrimitive(JSNumber_toDouble, JS2N)
    addPrimitive(JSString_toScalaString, JS2S)

    addPrimitive(JSDynamic_fromAny, ANY2DYN)
    addPrimitive(JSDynamic_window, WINDOW)

    addPrimitive(JSDynamic_selectDynamic, DYNSELECT)
    addPrimitive(JSDynamic_updateDynamic, DYNUPDATE)
    addPrimitive(JSDynamic_applyDynamic, DYNAPPLY)

    addPrimitive(JSDictionary_apply, DICT_SELECT)
    addPrimitive(JSDictionary_update, DICT_UPDATE)

    addPrimitive(JSArray_create, ARR_CREATE)
    addPrimitive(JSArray_apply, ARR_GET)
    addPrimitive(JSArray_update, ARR_SET)
  }

  def isJavaScriptPrimitive(code: Int) =
    code >= 300 && code < 340
}
