/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._

/** Core definitions for Scala.js
 *
 *  @author Sébastien Doeraene
 */
trait JSDefinitions { self: JSGlobalAddons =>
  import global._

  object jsDefinitions extends JSDefinitionsClass

  import definitions._
  import rootMirror._

  class JSDefinitionsClass {

    lazy val ScalaJSJSPackage = getPackage(newTermNameCached("scala.scalajs.js")) // compat 2.10/2.11
      lazy val JSPackage_typeOf   = getMemberMethod(ScalaJSJSPackage, newTermName("typeOf"))
      lazy val JSPackage_debugger = getMemberMethod(ScalaJSJSPackage, newTermName("debugger"))

    lazy val JSAnyClass       = getRequiredClass("scala.scalajs.js.Any")
    lazy val JSDynamicClass   = getRequiredClass("scala.scalajs.js.Dynamic")
      lazy val JSDynamic_selectDynamic = getMemberMethod(JSDynamicClass, newTermName("selectDynamic"))
      lazy val JSDynamic_updateDynamic = getMemberMethod(JSDynamicClass, newTermName("updateDynamic"))
      lazy val JSDynamic_applyDynamic  = getMemberMethod(JSDynamicClass, newTermName("applyDynamic"))
    lazy val JSDictionaryClass = getRequiredClass("scala.scalajs.js.Dictionary")
      lazy val JSDictionary_delete = getMemberMethod(JSDictionaryClass, newTermName("delete"))
    lazy val JSNumberClass    = getRequiredClass("scala.scalajs.js.Number")
    lazy val JSBooleanClass   = getRequiredClass("scala.scalajs.js.Boolean")
    lazy val JSStringClass    = getRequiredClass("scala.scalajs.js.String")
    lazy val JSUndefinedClass = getRequiredClass("scala.scalajs.js.Undefined")
    lazy val JSObjectClass    = getRequiredClass("scala.scalajs.js.Object")
    lazy val JSThisFunctionClass = getRequiredClass("scala.scalajs.js.ThisFunction")

    lazy val JSGlobalScopeClass = getRequiredClass("scala.scalajs.js.GlobalScope")

    lazy val JSArrayClass = getRequiredClass("scala.scalajs.js.Array")
      lazy val JSArray_apply  = getMemberMethod(JSArrayClass, newTermName("apply"))
      lazy val JSArray_update = getMemberMethod(JSArrayClass, newTermName("update"))

    lazy val JSFunctionClasses     = (0 to 22) map (n => getRequiredClass("scala.scalajs.js.Function"+n))
    lazy val JSThisFunctionClasses = (0 to 21) map (n => getRequiredClass("scala.scalajs.js.ThisFunction"+n))
    lazy val AllJSFunctionClasses  = JSFunctionClasses ++ JSThisFunctionClasses

    lazy val RuntimeExceptionClass    = requiredClass[RuntimeException]
    lazy val JavaScriptExceptionClass = getClassIfDefined("scala.scalajs.js.JavaScriptException")

    lazy val JSNameAnnotation          = getRequiredClass("scala.scalajs.js.annotation.JSName")
    lazy val JSBracketAccessAnnotation = getRequiredClass("scala.scalajs.js.annotation.JSBracketAccess")
    lazy val JSExportAnnotation        = getRequiredClass("scala.scalajs.js.annotation.JSExport")

    lazy val JSAnyTpe       = JSAnyClass.toTypeConstructor
    lazy val JSDynamicTpe   = JSDynamicClass.toTypeConstructor
    lazy val JSNumberTpe    = JSNumberClass.toTypeConstructor
    lazy val JSBooleanTpe   = JSBooleanClass.toTypeConstructor
    lazy val JSStringTpe    = JSStringClass.toTypeConstructor
    lazy val JSUndefinedTpe = JSUndefinedClass.toTypeConstructor
    lazy val JSObjectTpe    = JSObjectClass.toTypeConstructor

    lazy val JSGlobalScopeTpe = JSGlobalScopeClass.toTypeConstructor

    lazy val JSFunctionTpes = JSFunctionClasses.map(_.toTypeConstructor)

    lazy val JSAnyModule = JSAnyClass.companionModule
      lazy val JSAny_fromUnit    = getMemberMethod(JSAnyModule, newTermName("fromUnit"))
      lazy val JSAny_fromBoolean = getMemberMethod(JSAnyModule, newTermName("fromBoolean"))
      lazy val JSAny_fromByte    = getMemberMethod(JSAnyModule, newTermName("fromByte"))
      lazy val JSAny_fromShort   = getMemberMethod(JSAnyModule, newTermName("fromShort"))
      lazy val JSAny_fromInt     = getMemberMethod(JSAnyModule, newTermName("fromInt"))
      lazy val JSAny_fromFloat   = getMemberMethod(JSAnyModule, newTermName("fromFloat"))
      lazy val JSAny_fromDouble  = getMemberMethod(JSAnyModule, newTermName("fromDouble"))
      lazy val JSAny_fromString  = getMemberMethod(JSAnyModule, newTermName("fromString"))

      lazy val JSAny_fromTraversableOnce = getMemberMethod(JSAnyModule, newTermName("fromTraversableOnce"))

      def JSAny_fromFunction(arity: Int) = getMemberMethod(JSAnyModule, newTermName("fromFunction"+arity))

    lazy val JSDynamicModule = JSDynamicClass.companionModule
      lazy val JSDynamic_global      = getMemberMethod(JSDynamicModule, newTermName("global"))
      lazy val JSDynamic_newInstance = getMemberMethod(JSDynamicModule, newTermName("newInstance"))
    lazy val JSDynamicLiteral = getMemberModule(JSDynamicModule, newTermName("literal"))
      lazy val JSDynamicLiteral_applyDynamicNamed = getMemberMethod(JSDynamicLiteral, newTermName("applyDynamicNamed"))
      lazy val JSDynamicLiteral_applyDynamic = getMemberMethod(JSDynamicLiteral, newTermName("applyDynamic"))

    lazy val JSNumberModule = JSNumberClass.companionModule
      lazy val JSNumber_toDouble = getMemberMethod(JSNumberModule, newTermName("toDouble"))

    lazy val JSDictionaryModule = JSDictionaryClass.companionModule
      lazy val JSDictionary_propertiesOf = getMemberMethod(JSDictionaryModule, newTermName("propertiesOf"))

    lazy val JSBooleanModule = JSBooleanClass.companionModule
      lazy val JSBoolean_toBoolean = getMemberMethod(JSBooleanModule, newTermName("toBoolean"))

    lazy val JSStringModule = JSStringClass.companionModule
      lazy val JSString_toScalaString = getMemberMethod(JSStringModule, newTermName("toScalaString"))

    lazy val JSObjectModule = JSObjectClass.companionModule
      lazy val JSObject_hasProperty = getMemberMethod(JSObjectModule, newTermName("hasProperty"))

    lazy val JSArrayModule = JSArrayClass.companionModule
      lazy val JSArray_create = getMemberMethod(JSArrayModule, newTermName("apply"))

    lazy val JSThisFunctionModule = JSThisFunctionClass.companionModule
      def JSThisFunction_fromFunction(arity: Int) = getMemberMethod(JSThisFunctionModule, newTermName("fromFunction"+arity))

    lazy val RawJSTypeAnnot = getClassIfDefined("scala.scalajs.js.annotation.RawJSType")

    lazy val RuntimeLongClass  = getRequiredClass("scala.scalajs.runtime.Long")
    lazy val RuntimeLongModule = RuntimeLongClass.companionModule
      lazy val RuntimeLong_from = getMemberMethod(RuntimeLongModule, newTermName("fromRuntimeLong"))
      lazy val RuntimeLong_to   = getMemberMethod(RuntimeLongModule, newTermName("toRuntimeLong"))

    lazy val RuntimeStringClass = getRequiredClass("scala.scalajs.runtime.RuntimeString")
    lazy val RuntimeStringModule = RuntimeStringClass.companionModule

  }
}
