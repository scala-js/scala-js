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

  val FirstJSPrimitiveCode = 300

  val F2JS = FirstJSPrimitiveCode + 1 // FunctionN to js.FunctionN
  val F2JSTHIS = F2JS + 1             // FunctionN to js.ThisFunction{N-1}

  val DYNNEW = F2JSTHIS + 1 // Instantiate a new JavaScript object

  val ARR_CREATE = DYNNEW + 1 // js.Array.apply (array literal syntax)

  val TYPEOF = ARR_CREATE + 1 // typeof x
  val JS_NATIVE = TYPEOF + 1  // js.native. Marker method. Fails if tried to be emitted.

  val UNITVAL = JS_NATIVE + 1 // () value, which is undefined

  val CONSTRUCTOROF = UNITVAL + 1                                // runtime.constructorOf(clazz)
  val CREATE_INNER_JS_CLASS = CONSTRUCTOROF + 1                  // runtime.createInnerJSClass
  val CREATE_LOCAL_JS_CLASS = CREATE_INNER_JS_CLASS + 1          // runtime.createLocalJSClass
  val WITH_CONTEXTUAL_JS_CLASS_VALUE = CREATE_LOCAL_JS_CLASS + 1 // runtime.withContextualJSClassValue
  val LINKING_INFO = WITH_CONTEXTUAL_JS_CLASS_VALUE + 1          // runtime.linkingInfo

  val IN = LINKING_INFO + 1   // js.special.in
  val INSTANCEOF = IN + 1     // js.special.instanceof
  val DELETE = INSTANCEOF + 1 // js.special.delete
  val DEBUGGER = DELETE + 1   // js.special.debugger

  val LastJSPrimitiveCode = DEBUGGER

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

    addPrimitive(JSArray_create, ARR_CREATE)

    addPrimitive(JSPackage_typeOf, TYPEOF)
    addPrimitive(JSPackage_native, JS_NATIVE)

    addPrimitive(BoxedUnit_UNIT, UNITVAL)

    addPrimitive(Runtime_constructorOf, CONSTRUCTOROF)
    addPrimitive(Runtime_createInnerJSClass, CREATE_INNER_JS_CLASS)
    addPrimitive(Runtime_createLocalJSClass, CREATE_LOCAL_JS_CLASS)
    addPrimitive(Runtime_withContextualJSClassValue,
        WITH_CONTEXTUAL_JS_CLASS_VALUE)
    addPrimitive(Runtime_linkingInfo, LINKING_INFO)

    addPrimitive(Special_in, IN)
    addPrimitive(Special_instanceof, INSTANCEOF)
    addPrimitive(Special_delete, DELETE)
    addPrimitive(Special_debugger, DEBUGGER)
  }

  def isJavaScriptPrimitive(code: Int): Boolean =
    code >= FirstJSPrimitiveCode && code <= LastJSPrimitiveCode
}
