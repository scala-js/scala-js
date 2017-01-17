/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.core.compiler

import scala.tools.nsc._

/** Core definitions for Scala.js
 *
 *  @author Sébastien Doeraene
 */
trait JSDefinitions { self: JSGlobalAddons =>
  import global._

  // scalastyle:off line.size.limit

  object jsDefinitions extends JSDefinitionsClass // scalastyle:ignore

  import definitions._
  import rootMirror._

  class JSDefinitionsClass {

    lazy val ScalaJSJSPackage = getPackage(newTermNameCached("scala.scalajs.js")) // compat 2.10/2.11
      lazy val JSPackage_typeOf        = getMemberMethod(ScalaJSJSPackage, newTermName("typeOf"))
      lazy val JSPackage_constructorOf = getMemberMethod(ScalaJSJSPackage, newTermName("constructorOf"))
      lazy val JSPackage_debugger      = getMemberMethod(ScalaJSJSPackage, newTermName("debugger"))
      lazy val JSPackage_native        = getMemberMethod(ScalaJSJSPackage, newTermName("native"))
      lazy val JSPackage_undefined     = getMemberMethod(ScalaJSJSPackage, newTermName("undefined"))

    lazy val JSNativeAnnotation = getRequiredClass("scala.scalajs.js.native")

    lazy val JSAnyClass       = getRequiredClass("scala.scalajs.js.Any")
    lazy val JSDynamicClass   = getRequiredClass("scala.scalajs.js.Dynamic")
    lazy val JSDictionaryClass = getRequiredClass("scala.scalajs.js.Dictionary")
      lazy val JSDictionary_delete = getMemberMethod(JSDictionaryClass, newTermName("delete"))
    lazy val JSObjectClass    = getRequiredClass("scala.scalajs.js.Object")
    lazy val JSThisFunctionClass = getRequiredClass("scala.scalajs.js.ThisFunction")

    lazy val JSGlobalScopeClass = getRequiredClass("scala.scalajs.js.GlobalScope")

    lazy val UndefOrClass = getRequiredClass("scala.scalajs.js.UndefOr")
    lazy val UnionClass = getRequiredClass("scala.scalajs.js.$bar")

    lazy val JSArrayClass = getRequiredClass("scala.scalajs.js.Array")
      lazy val JSArray_apply  = getMemberMethod(JSArrayClass, newTermName("apply"))
      lazy val JSArray_update = getMemberMethod(JSArrayClass, newTermName("update"))

    lazy val JSFunctionClasses     = (0 to 22) map (n => getRequiredClass("scala.scalajs.js.Function"+n))
    lazy val JSThisFunctionClasses = (0 to 21) map (n => getRequiredClass("scala.scalajs.js.ThisFunction"+n))
    lazy val AllJSFunctionClasses  = JSFunctionClasses ++ JSThisFunctionClasses

    lazy val RuntimeExceptionClass    = requiredClass[RuntimeException]
    lazy val JavaScriptExceptionClass = getClassIfDefined("scala.scalajs.js.JavaScriptException")

    lazy val JSNameAnnotation          = getRequiredClass("scala.scalajs.js.annotation.JSName")
    lazy val JSFullNameAnnotation      = getRequiredClass("scala.scalajs.js.annotation.JSFullName")
    lazy val JSBracketAccessAnnotation = getRequiredClass("scala.scalajs.js.annotation.JSBracketAccess")
    lazy val JSBracketCallAnnotation   = getRequiredClass("scala.scalajs.js.annotation.JSBracketCall")
    lazy val JSExportAnnotation        = getRequiredClass("scala.scalajs.js.annotation.JSExport")
    lazy val JSExportDescendentObjectsAnnotation = getRequiredClass("scala.scalajs.js.annotation.JSExportDescendentObjects")
    lazy val JSExportDescendentClassesAnnotation = getRequiredClass("scala.scalajs.js.annotation.JSExportDescendentClasses")
    lazy val JSExportAllAnnotation     = getRequiredClass("scala.scalajs.js.annotation.JSExportAll")
    lazy val JSExportNamedAnnotation   = getRequiredClass("scala.scalajs.js.annotation.JSExportNamed")
    lazy val JSExportStaticAnnotation  = getRequiredClass("scala.scalajs.js.annotation.JSExportStatic")
    lazy val JSExportTopLevelAnnotation = getRequiredClass("scala.scalajs.js.annotation.JSExportTopLevel")
    lazy val JSImportAnnotation        = getRequiredClass("scala.scalajs.js.annotation.JSImport")
    lazy val JSGlobalScopeAnnotation   = getRequiredClass("scala.scalajs.js.annotation.JSGlobalScope")
    lazy val ScalaJSDefinedAnnotation  = getRequiredClass("scala.scalajs.js.annotation.ScalaJSDefined")
    lazy val SJSDefinedAnonymousClassAnnotation = getRequiredClass("scala.scalajs.js.annotation.SJSDefinedAnonymousClass")
    lazy val HasJSNativeLoadSpecAnnotation = getRequiredClass("scala.scalajs.js.annotation.internal.HasJSNativeLoadSpec")
    lazy val JSOptionalAnnotation      = getRequiredClass("scala.scalajs.js.annotation.internal.JSOptional")

    lazy val JavaDefaultMethodAnnotation = getRequiredClass("scala.scalajs.js.annotation.JavaDefaultMethod")

    lazy val JSImportNamespaceObject = getRequiredModule("scala.scalajs.js.annotation.JSImport.Namespace")

    lazy val JSAnyTpe    = JSAnyClass.toTypeConstructor
    lazy val JSObjectTpe = JSObjectClass.toTypeConstructor

    lazy val JSGlobalScopeTpe = JSGlobalScopeClass.toTypeConstructor

    lazy val JSFunctionTpes = JSFunctionClasses.map(_.toTypeConstructor)

    lazy val JSAnyModule = JSAnyClass.companionModule
      def JSAny_fromFunction(arity: Int): TermSymbol = getMemberMethod(JSAnyModule, newTermName("fromFunction"+arity))

    lazy val JSDynamicModule = JSDynamicClass.companionModule
      lazy val JSDynamic_newInstance = getMemberMethod(JSDynamicModule, newTermName("newInstance"))
    lazy val JSDynamicLiteral = getMemberModule(JSDynamicModule, newTermName("literal"))
      lazy val JSDynamicLiteral_applyDynamicNamed = getMemberMethod(JSDynamicLiteral, newTermName("applyDynamicNamed"))
      lazy val JSDynamicLiteral_applyDynamic = getMemberMethod(JSDynamicLiteral, newTermName("applyDynamic"))

    lazy val JSObjectModule = JSObjectClass.companionModule
      lazy val JSObject_hasProperty = getMemberMethod(JSObjectModule, newTermName("hasProperty"))
      lazy val JSObject_properties  = getMemberMethod(JSObjectModule, newTermName("properties"))

    lazy val JSArrayModule = JSArrayClass.companionModule
      lazy val JSArray_create = getMemberMethod(JSArrayModule, newTermName("apply"))

    lazy val JSThisFunctionModule = JSThisFunctionClass.companionModule
      def JSThisFunction_fromFunction(arity: Int): TermSymbol = getMemberMethod(JSThisFunctionModule, newTermName("fromFunction"+arity))

    lazy val JSConstructorTagModule = getRequiredModule("scala.scalajs.js.ConstructorTag")
      lazy val JSConstructorTag_materialize = getMemberMethod(JSConstructorTagModule, newTermName("materialize"))

    lazy val RawJSTypeAnnot = getRequiredClass("scala.scalajs.js.annotation.RawJSType")
    lazy val ExposedJSMemberAnnot = getRequiredClass("scala.scalajs.js.annotation.ExposedJSMember")

    lazy val RuntimeStringModule = getRequiredModule("scala.scalajs.runtime.RuntimeString")
    lazy val RuntimeStringModuleClass = RuntimeStringModule.moduleClass

    lazy val BooleanReflectiveCallClass = getRequiredClass("scala.scalajs.runtime.BooleanReflectiveCall")
    lazy val NumberReflectiveCallClass = getRequiredClass("scala.scalajs.runtime.NumberReflectiveCall")
    lazy val IntegerReflectiveCallClass = getRequiredClass("scala.scalajs.runtime.IntegerReflectiveCall")
    lazy val LongReflectiveCallClass = getRequiredClass("scala.scalajs.runtime.LongReflectiveCall")

    lazy val RuntimePackageModule = getPackageObject("scala.scalajs.runtime")
      lazy val Runtime_wrapJavaScriptException    = getMemberMethod(RuntimePackageModule, newTermName("wrapJavaScriptException"))
      lazy val Runtime_unwrapJavaScriptException  = getMemberMethod(RuntimePackageModule, newTermName("unwrapJavaScriptException"))
      lazy val Runtime_genTraversableOnce2jsArray = getMemberMethod(RuntimePackageModule, newTermName("genTraversableOnce2jsArray"))
      lazy val Runtime_jsTupleArray2jsObject      = getMemberMethod(RuntimePackageModule, newTermName("jsTupleArray2jsObject"))
      lazy val Runtime_constructorOf              = getMemberMethod(RuntimePackageModule, newTermName("constructorOf"))
      lazy val Runtime_newConstructorTag          = getMemberMethod(RuntimePackageModule, newTermName("newConstructorTag"))
      lazy val Runtime_propertiesOf               = getMemberMethod(RuntimePackageModule, newTermName("propertiesOf"))
      lazy val Runtime_linkingInfo                = getMemberMethod(RuntimePackageModule, newTermName("linkingInfo"))

    lazy val WrappedArrayClass = getRequiredClass("scala.scalajs.js.WrappedArray")
      lazy val WrappedArray_ctor = WrappedArrayClass.primaryConstructor

    // This is a def, since similar symbols (arrayUpdateMethod, etc.) are in runDefinitions
    // (rather than definitions) and we weren't sure if it is safe to make this a lazy val
    def ScalaRunTime_isArray: Symbol = getMemberMethod(ScalaRunTimeModule, newTermName("isArray")).suchThat(_.tpe.params.size == 2)

    lazy val BoxesRunTime_boxToCharacter = getMemberMethod(BoxesRunTimeModule, newTermName("boxToCharacter"))
    lazy val BoxesRunTime_unboxToChar    = getMemberMethod(BoxesRunTimeModule, newTermName("unboxToChar"))

    lazy val ReflectModule = getRequiredModule("scala.scalajs.reflect.Reflect")
      lazy val Reflect_registerLoadableModuleClass = getMemberMethod(ReflectModule, newTermName("registerLoadableModuleClass"))
      lazy val Reflect_registerInstantiatableClass = getMemberMethod(ReflectModule, newTermName("registerInstantiatableClass"))

    lazy val EnableReflectiveInstantiationAnnotation = getRequiredClass("scala.scalajs.reflect.annotation.EnableReflectiveInstantiation")

  }

  // scalastyle:on line.size.limit
}
