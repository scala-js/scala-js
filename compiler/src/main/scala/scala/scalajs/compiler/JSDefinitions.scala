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
    lazy val MaybeJSAnyClass = getClassIfDefined("scala.scalajs.js.Any")
    lazy val isScalaJSDefined = MaybeJSAnyClass != NoSymbol
    lazy val MaybeJSAnyTpe = if (isScalaJSDefined) MaybeJSAnyClass.toTypeConstructor else NoType

    lazy val JSAnyClass       = getRequiredClass("scala.scalajs.js.Any")
    lazy val JSDynamicClass   = getRequiredClass("scala.scalajs.js.Dynamic")
      lazy val JSDynamic_selectDynamic = getMemberMethod(JSDynamicClass, newTermName("selectDynamic"))
      lazy val JSDynamic_updateDynamic = getMemberMethod(JSDynamicClass, newTermName("updateDynamic"))
      lazy val JSDynamic_applyDynamic  = getMemberMethod(JSDynamicClass, newTermName("applyDynamic"))
    lazy val JSDictionaryClass = getRequiredClass("scala.scalajs.js.Dictionary")
      lazy val JSDictionary_apply  = getMemberMethod(JSDictionaryClass, newTermName("apply"))
      lazy val JSDictionary_update = getMemberMethod(JSDictionaryClass, newTermName("update"))
    lazy val JSNumberClass    = getRequiredClass("scala.scalajs.js.Number")
    lazy val JSBooleanClass   = getRequiredClass("scala.scalajs.js.Boolean")
    lazy val JSStringClass    = getRequiredClass("scala.scalajs.js.String")
    lazy val JSUndefinedClass = getRequiredClass("scala.scalajs.js.Undefined")
    lazy val JSObjectClass    = getRequiredClass("scala.scalajs.js.Object")

    lazy val JSGlobalScopeClass = getRequiredClass("scala.scalajs.js.GlobalScope")

    lazy val JSArrayClass = getRequiredClass("scala.scalajs.js.Array")
      lazy val JSArray_apply  = getMemberMethod(JSArrayClass, newTermName("apply"))
      lazy val JSArray_update = getMemberMethod(JSArrayClass, newTermName("update"))

    lazy val RuntimeExceptionClass    = requiredClass[RuntimeException]
    lazy val JavaScriptExceptionClass = getClassIfDefined("scala.scalajs.js.JavaScriptException")

    lazy val JSNameAnnotation          = getRequiredClass("scala.scalajs.js.annotation.JSName")
    lazy val JSBracketAccessAnnotation = getRequiredClass("scala.scalajs.js.annotation.JSBracketAccess")

    lazy val JSAnyTpe       = JSAnyClass.toTypeConstructor
    lazy val JSDynamicTpe   = JSDynamicClass.toTypeConstructor
    lazy val JSNumberTpe    = JSNumberClass.toTypeConstructor
    lazy val JSBooleanTpe   = JSBooleanClass.toTypeConstructor
    lazy val JSStringTpe    = JSStringClass.toTypeConstructor
    lazy val JSUndefinedTpe = JSUndefinedClass.toTypeConstructor
    lazy val JSObjectTpe    = JSObjectClass.toTypeConstructor

    lazy val JSGlobalScopeTpe = JSGlobalScopeClass.toTypeConstructor

    lazy val JSAnyModule = JSAnyClass.companionModule
      lazy val JSAny_fromUnit    = getMemberMethod(JSAnyModule, newTermName("fromUnit"))
      lazy val JSAny_fromBoolean = getMemberMethod(JSAnyModule, newTermName("fromBoolean"))
      lazy val JSAny_fromByte    = getMemberMethod(JSAnyModule, newTermName("fromByte"))
      lazy val JSAny_fromShort   = getMemberMethod(JSAnyModule, newTermName("fromShort"))
      lazy val JSAny_fromInt     = getMemberMethod(JSAnyModule, newTermName("fromInt"))
      lazy val JSAny_fromLong    = getMemberMethod(JSAnyModule, newTermName("fromLong"))
      lazy val JSAny_fromFloat   = getMemberMethod(JSAnyModule, newTermName("fromFloat"))
      lazy val JSAny_fromDouble  = getMemberMethod(JSAnyModule, newTermName("fromDouble"))
      lazy val JSAny_fromString  = getMemberMethod(JSAnyModule, newTermName("fromString"))

      lazy val JSAny_fromTraversableOnce = getMemberMethod(JSAnyModule, newTermName("fromTraversableOnce"))

      def JSAny_fromFunction(arity: Int) = getMemberMethod(JSAnyModule, newTermName("fromFunction"+arity))

    lazy val JSDynamicModule = JSDynamicClass.companionModule
      lazy val JSDynamic_global      = getMemberMethod(JSDynamicModule, newTermName("global"))
      lazy val JSDynamic_newInstance = getMemberMethod(JSDynamicModule, newTermName("newInstance"))

    lazy val JSNumberModule = JSNumberClass.companionModule
      lazy val JSNumber_toDouble = getMemberMethod(JSNumberModule, newTermName("toDouble"))

    lazy val JSDictionaryModule = JSDictionaryClass.companionModule
      lazy val JSDictionary_propertiesOf = getMemberMethod(JSDictionaryModule, newTermName("propertiesOf"))

    lazy val JSBooleanModule = JSBooleanClass.companionModule
      lazy val JSBoolean_toBoolean = getMemberMethod(JSBooleanModule, newTermName("toBoolean"))

    lazy val JSStringModule = JSStringClass.companionModule
      lazy val JSString_toScalaString = getMemberMethod(JSStringModule, newTermName("toScalaString"))

    lazy val JSArrayModule = JSArrayClass.companionModule
      lazy val JSArray_create = getMemberMethod(JSArrayModule, newTermName("apply"))

    lazy val RawJSTypeAnnot = getClassIfDefined("scala.scalajs.js.annotation.RawJSType")
  }
}
