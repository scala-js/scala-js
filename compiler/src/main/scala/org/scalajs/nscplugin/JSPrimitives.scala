/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.nscplugin

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
  import jsDefinitions._
  import scalaPrimitives._

  final val FirstJSPrimitiveCode = 300

  final val DYNNEW = FirstJSPrimitiveCode + 1 // Instantiate a new JavaScript object

  final val ARR_CREATE = DYNNEW + 1 // js.Array.apply (array literal syntax)

  final val TYPEOF = ARR_CREATE + 1 // typeof x
  final val JS_NATIVE = TYPEOF + 1  // js.native. Marker method. Fails if tried to be emitted.

  final val UNITVAL = JS_NATIVE + 1 // () value, which is undefined

  final val JS_IMPORT = UNITVAL + 1 // js.import.apply(specifier)

  final val CONSTRUCTOROF = JS_IMPORT + 1                              // runtime.constructorOf(clazz)
  final val CREATE_INNER_JS_CLASS = CONSTRUCTOROF + 1                  // runtime.createInnerJSClass
  final val CREATE_LOCAL_JS_CLASS = CREATE_INNER_JS_CLASS + 1          // runtime.createLocalJSClass
  final val WITH_CONTEXTUAL_JS_CLASS_VALUE = CREATE_LOCAL_JS_CLASS + 1 // runtime.withContextualJSClassValue
  final val LINKING_INFO = WITH_CONTEXTUAL_JS_CLASS_VALUE + 1          // runtime.linkingInfo
  final val IDENTITY_HASH_CODE = LINKING_INFO + 1                      // runtime.identityHashCode

  final val STRICT_EQ = IDENTITY_HASH_CODE + 1 // js.special.strictEquals
  final val IN = STRICT_EQ + 1                 // js.special.in
  final val INSTANCEOF = IN + 1                // js.special.instanceof
  final val DELETE = INSTANCEOF + 1            // js.special.delete
  final val FORIN = DELETE + 1                 // js.special.forin
  final val DEBUGGER = FORIN + 1               // js.special.debugger

  final val LastJSPrimitiveCode = DEBUGGER

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

  private val scalaJSPrimitives = mutable.Map.empty[Symbol, Int]

  private def initWithPrimitives(addPrimitive: (Symbol, Int) => Unit): Unit = {
    addPrimitive(JSDynamic_newInstance, DYNNEW)

    addPrimitive(JSArray_create, ARR_CREATE)

    addPrimitive(JSPackage_typeOf, TYPEOF)
    addPrimitive(JSPackage_native, JS_NATIVE)

    addPrimitive(BoxedUnit_UNIT, UNITVAL)

    addPrimitive(JSImport_apply, JS_IMPORT)

    addPrimitive(Runtime_constructorOf, CONSTRUCTOROF)
    addPrimitive(Runtime_createInnerJSClass, CREATE_INNER_JS_CLASS)
    addPrimitive(Runtime_createLocalJSClass, CREATE_LOCAL_JS_CLASS)
    addPrimitive(Runtime_withContextualJSClassValue,
        WITH_CONTEXTUAL_JS_CLASS_VALUE)
    addPrimitive(Runtime_linkingInfo, LINKING_INFO)
    addPrimitive(Runtime_identityHashCode, IDENTITY_HASH_CODE)

    addPrimitive(Special_strictEquals, STRICT_EQ)
    addPrimitive(Special_in, IN)
    addPrimitive(Special_instanceof, INSTANCEOF)
    addPrimitive(Special_delete, DELETE)
    addPrimitive(Special_forin, FORIN)
    addPrimitive(Special_debugger, DEBUGGER)
  }

  def isJavaScriptPrimitive(code: Int): Boolean =
    code >= FirstJSPrimitiveCode && code <= LastJSPrimitiveCode
}
