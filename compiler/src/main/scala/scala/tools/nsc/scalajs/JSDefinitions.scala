/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package scalajs

import scala.reflect.internal.SymbolTable

trait JSDefinitions { self: SymbolTable =>
  object jsDefinitions extends JSDefinitionsClass

  import definitions._

  import rootMirror.{
    getClassByName, getRequiredClass, getClassIfDefined,
    getModuleByName, getRequiredModule, getModuleIfDefined,
    getRequiredPackage,
    getPackageObject, getPackageObjectIfDefined,
    requiredClass, requiredModule
  }

  class JSDefinitionsClass {
    lazy val MaybeJSAnyClass = getClassIfDefined("scala.js.JSAny")
    lazy val isScalaJSDefined = MaybeJSAnyClass != NoSymbol
    lazy val MaybeJSAnyTpe = if (isScalaJSDefined) MaybeJSAnyClass.toTypeConstructor else NoType

    lazy val JSAnyClass       = getRequiredClass("scala.js.JSAny")
    lazy val JSDynamicClass   = getRequiredClass("scala.js.JSDynamic")
      lazy val JSDynamic_selectDynamic = getMemberMethod(JSDynamicClass, newTermName("selectDynamic"))
      lazy val JSDynamic_updateDynamic = getMemberMethod(JSDynamicClass, newTermName("updateDynamic"))
      lazy val JSDynamic_applyDynamic  = getMemberMethod(JSDynamicClass, newTermName("applyDynamic"))
    lazy val JSNumberClass    = getRequiredClass("scala.js.JSNumber")
    lazy val JSBooleanClass   = getRequiredClass("scala.js.JSBoolean")
    lazy val JSStringClass    = getRequiredClass("scala.js.JSString")
    lazy val JSUndefinedClass = getRequiredClass("scala.js.JSUndefined")
    lazy val JSObjectClass    = getRequiredClass("scala.js.JSObject")

    lazy val JSAnyTpe       = JSAnyClass.toTypeConstructor
    lazy val JSDynamicTpe   = JSDynamicClass.toTypeConstructor
    lazy val JSNumberTpe    = JSNumberClass.toTypeConstructor
    lazy val JSBooleanTpe   = JSBooleanClass.toTypeConstructor
    lazy val JSStringTpe    = JSStringClass.toTypeConstructor
    lazy val JSUndefinedTpe = JSUndefinedClass.toTypeConstructor
    lazy val JSObjectTpe    = JSObjectClass.toTypeConstructor

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

    lazy val JSDynamicModule = JSDynamicClass.companionModule
      lazy val JSDynamic_fromAny = getMemberMethod(JSDynamicModule, newTermName("fromAny"))
      lazy val JSDynamic_window  = getMemberMethod(JSDynamicModule, newTermName("window"))

    lazy val JSNumberModule = JSNumberClass.companionModule
      lazy val JSNumber_toDouble = getMemberMethod(JSNumberModule, newTermName("toDouble"))

    lazy val JSBooleanModule = JSBooleanClass.companionModule
      lazy val JSBoolean_toBoolean = getMemberMethod(JSBooleanModule, newTermName("toBoolean"))

    lazy val JSStringModule = JSStringClass.companionModule
      lazy val JSString_toScalaString = getMemberMethod(JSStringModule, newTermName("toScalaString"))

    lazy val JSObjectModule = JSObjectClass.companionModule
      lazy val JSObject_newEmpty = getMemberMethod(JSObjectModule, newTermName("newEmpty"))
  }
}
