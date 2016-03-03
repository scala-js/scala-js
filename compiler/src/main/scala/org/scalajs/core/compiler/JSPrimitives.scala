/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.core.compiler

import scala.tools.nsc._

import scala.collection.mutable

/** Extension of ScalaPrimitives for primitives only relevant to the JS backend
 *
 *  @author Sébastie Doeraene
 */
abstract class JSPrimitives {
  val global: Global

  type ThisJSGlobalAddons = JSGlobalAddons {
    val global: JSPrimitives.this.global.type
  }

  val jsAddons: ThisJSGlobalAddons

  import global._
  import jsAddons._
  import definitions._
  import rootMirror._
  import jsDefinitions._
  import scalaPrimitives._

  val F2JS = 305     // FunctionN to js.FunctionN
  val F2JSTHIS = 306 // FunctionN to js.ThisFunction{N-1}

  val DYNNEW = 321 // Instantiate a new JavaScript object

  val DYNLIT = 334    // js.Dynamic.literal.applyDynamic{,Named}

  val DICT_DEL = 335   // js.Dictionary.delete

  val ARR_CREATE = 337 // js.Array.apply (array literal syntax)

  val TYPEOF = 344    // typeof x
  val DEBUGGER = 345  // js.debugger()
  val HASPROP = 346   // js.Object.hasProperty(o, p), equiv to `p in o` in JS
  val OBJPROPS = 347  // js.Object.properties(o), equiv to `for (p in o)` in JS
  val JS_NATIVE = 348 // js.native. Marker method. Fails if tried to be emitted.

  val UNITVAL = 349 // () value, which is undefined

  val CONSTRUCTOROF = 352 // runtime.constructorOf(clazz)
  val LINKING_INFO = 354  // $linkingInfo

  /** Initialize the map of primitive methods (for GenJSCode) */
  def init(): Unit = initWithPrimitives(addPrimitive)

  /** Init the map of primitive methods for Scala.js (for PrepJSInterop) */
  def initPrepJSPrimitives(): Unit = {
    scalaJSPrimitives.clear()
    initWithPrimitives(scalaJSPrimitives.put)
  }

  /** Only call from PrepJSInterop. In GenJSCode, use
   *  scalaPrimitives.isPrimitive instead
   */
  def isJavaScriptPrimitive(sym: Symbol): Boolean =
    scalaJSPrimitives.contains(sym)

  /** For a primitive, is it one for which we should emit its body anyway? */
  def shouldEmitPrimitiveBody(sym: Symbol): Boolean = {
    /* No @switch because some Scala 2.11 versions erroneously report a
     * warning for switch matches with less than 3 non-default cases.
     */
    scalaPrimitives.getPrimitive(sym) match {
      case F2JS | F2JSTHIS => true
      case _               => false
    }
  }

  private val scalaJSPrimitives = mutable.Map.empty[Symbol, Int]

  private def initWithPrimitives(addPrimitive: (Symbol, Int) => Unit): Unit = {
    for (i <- 0 to 22)
      addPrimitive(JSAny_fromFunction(i), F2JS)
    for (i <- 1 to 22)
      addPrimitive(JSThisFunction_fromFunction(i), F2JSTHIS)

    addPrimitive(JSDynamic_newInstance, DYNNEW)

    addPrimitive(JSDynamicLiteral_applyDynamicNamed, DYNLIT)
    addPrimitive(JSDynamicLiteral_applyDynamic, DYNLIT)

    addPrimitive(JSDictionary_delete, DICT_DEL)

    addPrimitive(JSArray_create, ARR_CREATE)

    addPrimitive(JSPackage_typeOf, TYPEOF)
    addPrimitive(JSPackage_debugger, DEBUGGER)
    addPrimitive(JSPackage_native, JS_NATIVE)

    addPrimitive(JSObject_hasProperty, HASPROP)
    addPrimitive(JSObject_properties, OBJPROPS)

    addPrimitive(BoxedUnit_UNIT, UNITVAL)

    addPrimitive(Runtime_constructorOf, CONSTRUCTOROF)
    addPrimitive(Runtime_linkingInfo, LINKING_INFO)
  }

  def isJavaScriptPrimitive(code: Int): Boolean =
    code >= 300 && code < 360
}
