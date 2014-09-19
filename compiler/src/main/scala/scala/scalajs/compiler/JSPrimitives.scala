/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._

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

  val RTJ2J = 338 // Runtime Long to Long
  val J2RTJ = 339 // Long to Runtime Long

  val NTR_MOD_SUFF  = 340 // scala.reflect.NameTransformer.MODULE_SUFFIX_STRING
  val NTR_NAME_JOIN = 341 // scala.relfect.NameTransformer.NAME_JOIN_STRING

  val UNDEFVAL = 342 // js.undefined
  val ISUNDEF = 343  // js.isUndefined
  val TYPEOF = 344   // typeof x
  val DEBUGGER = 345 // js.debugger()
  val HASPROP = 346  // js.Object.hasProperty(o, p), equiv to `p in o` in JS
  val OBJPROPS = 347 // js.Object.properties(o), equiv to `for (p in o)` in JS

  val UNITVAL = 348  // () value, which is undefined
  val UNITTYPE = 349 // BoxedUnit.TYPE (== classOf[Unit])

  val ARRAYCOPY = 350  // System.arraycopy
  val IDHASHCODE = 351 // System.identityHashCode

  val ENV_INFO = 352  // __ScalaJSEnv via helper

  val AB2TA = 370 // scala.Array[Byte] to TypedArray
  val AS2TA = 371 // scala.Array[Short] to TypedArray
  val AC2TA = 372 // scala.Array[Char] to TypedArray
  val AI2TA = 373 // scala.Array[Int] to TypedArray
  val AF2TA = 374 // scala.Array[Float] to TypedArray
  val AD2TA = 375 // scala.Array[Double] to TypedArray

  val TA2AB = 376 // TypedArray to scala.Array[Byte]
  val TA2AS = 377 // TypedArray to scala.Array[Short]
  val TA2AC = 378 // TypedArray to scala.Array[Char]
  val TA2AI = 379 // TypedArray to scala.Array[Int]
  val TA2AF = 380 // TypedArray to scala.Array[Float]
  val TA2AD = 381 // TypedArray to scala.Array[Double]

  /** Initialize the map of primitive methods */
  def init(): Unit = {

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

    addPrimitive(RuntimeLong_from, RTJ2J)
    addPrimitive(RuntimeLong_to, J2RTJ)

    val ntModule = getRequiredModule("scala.reflect.NameTransformer")

    addPrimitive(getMember(ntModule, newTermName("MODULE_SUFFIX_STRING")), NTR_MOD_SUFF)
    addPrimitive(getMember(ntModule, newTermName("NAME_JOIN_STRING")), NTR_NAME_JOIN)

    addPrimitive(JSPackage_typeOf, TYPEOF)
    addPrimitive(JSPackage_debugger, DEBUGGER)
    addPrimitive(JSPackage_undefined, UNDEFVAL)
    addPrimitive(JSPackage_isUndefined, ISUNDEF)

    addPrimitive(JSObject_hasProperty, HASPROP)
    addPrimitive(JSObject_properties, OBJPROPS)

    addPrimitive(BoxedUnit_UNIT, UNITVAL)
    addPrimitive(BoxedUnit_TYPE, UNITTYPE)

    val SystemModule = getRequiredModule("java.lang.System")
    addPrimitive(getMember(SystemModule, newTermName("arraycopy")), ARRAYCOPY)
    addPrimitive(getMember(SystemModule, newTermName("identityHashCode")), IDHASHCODE)

    addPrimitive(getMember(RuntimePackageModule,
        newTermName("environmentInfo")), ENV_INFO)

    val typedArrayPO = getPackageObject("scala.scalajs.js.typedarray")
    addPrimitive(getMember(typedArrayPO, newTermName("byteArray2Int8Array")), AB2TA)
    addPrimitive(getMember(typedArrayPO, newTermName("shortArray2Int16Array")), AS2TA)
    addPrimitive(getMember(typedArrayPO, newTermName("charArray2Uint16Array")), AC2TA)
    addPrimitive(getMember(typedArrayPO, newTermName("intArray2Int32Array")), AI2TA)
    addPrimitive(getMember(typedArrayPO, newTermName("floatArray2Float32Array")), AF2TA)
    addPrimitive(getMember(typedArrayPO, newTermName("doubleArray2Float64Array")), AD2TA)

    addPrimitive(getMember(typedArrayPO, newTermName("int8Array2ByteArray")), TA2AB)
    addPrimitive(getMember(typedArrayPO, newTermName("int16Array2ShortArray")), TA2AS)
    addPrimitive(getMember(typedArrayPO, newTermName("uint16Array2CharArray")), TA2AC)
    addPrimitive(getMember(typedArrayPO, newTermName("int32Array2IntArray")), TA2AI)
    addPrimitive(getMember(typedArrayPO, newTermName("float32Array2FloatArray")), TA2AF)
    addPrimitive(getMember(typedArrayPO, newTermName("float64Array2DoubleArray")), TA2AD)
  }

  def isJavaScriptPrimitive(code: Int) =
    code >= 300 && code < 390
}
