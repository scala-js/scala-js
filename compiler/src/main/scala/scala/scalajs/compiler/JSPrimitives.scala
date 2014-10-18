/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

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

  // Conversions from Scala types to JS types
  val V2JS = 300 // Unit
  val Z2JS = 301 // Boolean
  //val C2JS = 302 // Char
  val N2JS = 303 // Number (any numeric type except for Long)
  val S2JS = 304 // String
  val F2JS = 305 // FunctionN
  val F2JSTHIS = 306 // ThisFunctionN

  // Conversions from JS types to Scala types
  val JS2Z = 311 // Boolean
  //val JS2C = 312 // Char
  val JS2N = 313 // Number (any numeric type)
  val JS2S = 314 // String

  val GETGLOBAL = 320 // Get the top-level object (`window` in browsers)
  val DYNNEW = 321    // Instantiate a new JavaScript object

  val DYNSELECT = 330 // js.Dynamic.selectDynamic
  val DYNUPDATE = 331 // js.Dynamic.updateDynamic
  val DYNAPPLY = 332  // js.Dynamic.applyDynamic
  val DYNLITN = 333   // js.Dynamic.literal.applyDynamicNamed
  val DYNLIT = 334    // js.Dynamic.literal.applyDynamic

  val DICT_DEL = 335   // js.Dictionary.delete

  val ARR_CREATE = 337 // js.Array.apply (array literal syntax)

  val NTR_MOD_SUFF  = 340 // scala.reflect.NameTransformer.MODULE_SUFFIX_STRING
  val NTR_NAME_JOIN = 341 // scala.relfect.NameTransformer.NAME_JOIN_STRING

  val UNDEFVAL = 342  // js.undefined
  val ISUNDEF = 343   // js.isUndefined
  val TYPEOF = 344    // typeof x
  val DEBUGGER = 345  // js.debugger()
  val HASPROP = 346   // js.Object.hasProperty(o, p), equiv to `p in o` in JS
  val OBJPROPS = 347  // js.Object.properties(o), equiv to `for (p in o)` in JS
  val JS_NATIVE = 348 // js.native. Marker method. Fails if tried to be emitted.

  val UNITVAL = 349  // () value, which is undefined
  val UNITTYPE = 350 // BoxedUnit.TYPE (== classOf[Unit])

  val IDHASHCODE = 352 // System.identityHashCode

  val ENV_INFO = 353  // __ScalaJSEnv via helper

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
    addPrimitive(JSAny_fromUnit, V2JS)
    addPrimitive(JSAny_fromBoolean, Z2JS)
    addPrimitive(JSAny_fromByte, N2JS)
    addPrimitive(JSAny_fromShort, N2JS)
    addPrimitive(JSAny_fromInt, N2JS)
    addPrimitive(JSAny_fromFloat, N2JS)
    addPrimitive(JSAny_fromDouble, N2JS)
    addPrimitive(JSAny_fromString, S2JS)

    for (i <- 0 to 22)
      addPrimitive(JSAny_fromFunction(i), F2JS)
    for (i <- 1 to 22)
      addPrimitive(JSThisFunction_fromFunction(i), F2JSTHIS)

    addPrimitive(JSBoolean_toBoolean, JS2Z)
    addPrimitive(JSNumber_toDouble, JS2N)
    addPrimitive(JSString_toScalaString, JS2S)

    addPrimitive(JSDynamic_global, GETGLOBAL)
    addPrimitive(JSDynamic_newInstance, DYNNEW)

    addPrimitive(JSDynamic_selectDynamic, DYNSELECT)
    addPrimitive(JSDynamic_updateDynamic, DYNUPDATE)
    addPrimitive(JSDynamic_applyDynamic, DYNAPPLY)
    addPrimitive(JSDynamicLiteral_applyDynamicNamed, DYNLITN)
    addPrimitive(JSDynamicLiteral_applyDynamic, DYNLIT)

    addPrimitive(JSDictionary_delete, DICT_DEL)

    addPrimitive(JSArray_create, ARR_CREATE)

    val ntModule = getRequiredModule("scala.reflect.NameTransformer")

    addPrimitive(getMember(ntModule, newTermName("MODULE_SUFFIX_STRING")), NTR_MOD_SUFF)
    addPrimitive(getMember(ntModule, newTermName("NAME_JOIN_STRING")), NTR_NAME_JOIN)

    addPrimitive(JSPackage_typeOf, TYPEOF)
    addPrimitive(JSPackage_debugger, DEBUGGER)
    addPrimitive(JSPackage_undefined, UNDEFVAL)
    addPrimitive(JSPackage_isUndefined, ISUNDEF)
    addPrimitive(JSPackage_native, JS_NATIVE)

    addPrimitive(JSObject_hasProperty, HASPROP)
    addPrimitive(JSObject_properties, OBJPROPS)

    addPrimitive(BoxedUnit_UNIT, UNITVAL)
    addPrimitive(BoxedUnit_TYPE, UNITTYPE)

    val SystemModule = getRequiredModule("java.lang.System")
    addPrimitive(getMember(SystemModule, newTermName("identityHashCode")), IDHASHCODE)

    addPrimitive(getMember(RuntimePackageModule,
        newTermName("environmentInfo")), ENV_INFO)
  }

  def isJavaScriptPrimitive(code: Int) =
    code >= 300 && code < 360
}
