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

/** Core definitions for Scala.js
 *
 *  @author Sébastien Doeraene
 */
trait JSDefinitions {
  val global: Global

  import global._

  // scalastyle:off line.size.limit

  object jsDefinitions extends JSDefinitionsClass

  import definitions._
  import rootMirror._

  class JSDefinitionsClass {

    lazy val HackedStringClass = getClassIfDefined("java.lang._String")
    lazy val HackedStringModClass = getModuleIfDefined("java.lang._String").moduleClass

    lazy val JavaLangVoidClass = getRequiredClass("java.lang.Void")

    lazy val BoxedUnitModClass = BoxedUnitModule.moduleClass

    lazy val ScalaJSJSPackageModule = getPackageObject("scala.scalajs.js")
      lazy val JSPackage_typeOf        = getMemberMethod(ScalaJSJSPackageModule, newTermName("typeOf"))
      lazy val JSPackage_constructorOf = getMemberMethod(ScalaJSJSPackageModule, newTermName("constructorOf"))
      lazy val JSPackage_native        = getMemberMethod(ScalaJSJSPackageModule, newTermName("native"))
      lazy val JSPackage_undefined     = getMemberMethod(ScalaJSJSPackageModule, newTermName("undefined"))
      lazy val JSPackage_dynamicImport = getMemberMethod(ScalaJSJSPackageModule, newTermName("dynamicImport"))

    lazy val JSNativeAnnotation = getRequiredClass("scala.scalajs.js.native")

    lazy val JSAnyClass       = getRequiredClass("scala.scalajs.js.Any")
    lazy val JSDynamicClass   = getRequiredClass("scala.scalajs.js.Dynamic")
    lazy val JSObjectClass    = getRequiredClass("scala.scalajs.js.Object")
    lazy val JSFunctionClass = getRequiredClass("scala.scalajs.js.Function")
    lazy val JSThisFunctionClass = getRequiredClass("scala.scalajs.js.ThisFunction")

    lazy val UnionClass = getRequiredClass("scala.scalajs.js.$bar")

    lazy val JSArrayClass = getRequiredClass("scala.scalajs.js.Array")

    lazy val JavaScriptExceptionClass = getClassIfDefined("scala.scalajs.js.JavaScriptException")

    lazy val JSNameAnnotation          = getRequiredClass("scala.scalajs.js.annotation.JSName")
    lazy val JSBracketAccessAnnotation = getRequiredClass("scala.scalajs.js.annotation.JSBracketAccess")
    lazy val JSBracketCallAnnotation   = getRequiredClass("scala.scalajs.js.annotation.JSBracketCall")
    lazy val JSExportAnnotation        = getRequiredClass("scala.scalajs.js.annotation.JSExport")
    lazy val JSExportAllAnnotation     = getRequiredClass("scala.scalajs.js.annotation.JSExportAll")
    lazy val JSExportStaticAnnotation  = getRequiredClass("scala.scalajs.js.annotation.JSExportStatic")
    lazy val JSExportTopLevelAnnotation = getRequiredClass("scala.scalajs.js.annotation.JSExportTopLevel")
    lazy val JSImportAnnotation        = getRequiredClass("scala.scalajs.js.annotation.JSImport")
    lazy val JSGlobalAnnotation        = getRequiredClass("scala.scalajs.js.annotation.JSGlobal")
    lazy val JSGlobalScopeAnnotation   = getRequiredClass("scala.scalajs.js.annotation.JSGlobalScope")
    lazy val JSOperatorAnnotation      = getRequiredClass("scala.scalajs.js.annotation.JSOperator")

    lazy val LinkTimePropertyAnnotation = getRequiredClass("scala.scalajs.js.annotation.linkTimeProperty")

    lazy val JSImportNamespaceObject = getRequiredModule("scala.scalajs.js.annotation.JSImport.Namespace")

    lazy val ExposedJSMemberAnnot = getRequiredClass("scala.scalajs.js.annotation.internal.ExposedJSMember")
    lazy val JSOptionalAnnotation = getRequiredClass("scala.scalajs.js.annotation.internal.JSOptional")
    lazy val JSTypeAnnot = getRequiredClass("scala.scalajs.js.annotation.internal.JSType")
    lazy val WasPublicBeforeTyperClass = getRequiredClass("scala.scalajs.js.annotation.internal.WasPublicBeforeTyper")

    lazy val JSDynamicModule = JSDynamicClass.companionModule
      lazy val JSDynamic_newInstance = getMemberMethod(JSDynamicModule, newTermName("newInstance"))
    lazy val JSDynamicLiteral = getMemberModule(JSDynamicModule, newTermName("literal"))
      lazy val JSDynamicLiteral_applyDynamicNamed = getMemberMethod(JSDynamicLiteral, newTermName("applyDynamicNamed"))
      lazy val JSDynamicLiteral_applyDynamic = getMemberMethod(JSDynamicLiteral, newTermName("applyDynamic"))

    lazy val JSArrayModule = JSArrayClass.companionModule
      lazy val JSArray_create = getMemberMethod(JSArrayModule, newTermName("apply"))

    lazy val JSConstructorTagModule = getRequiredModule("scala.scalajs.js.ConstructorTag")
      lazy val JSConstructorTag_materialize = getMemberMethod(JSConstructorTagModule, newTermName("materialize"))

    lazy val JSNewModule = getRequiredModule("scala.scalajs.js.new")
    lazy val JSNewModuleClass = JSNewModule.moduleClass
      lazy val JSNew_target = getMemberMethod(JSNewModuleClass, newTermName("target"))

    lazy val JSImportModule = getRequiredModule("scala.scalajs.js.import")
    lazy val JSImportModuleClass = JSImportModule.moduleClass
      lazy val JSImport_apply = getMemberMethod(JSImportModuleClass, nme.apply)
      lazy val JSImport_meta = getMemberMethod(JSImportModuleClass, newTermName("meta"))

    lazy val SpecialPackageModule = getPackageObject("scala.scalajs.js.special")
      lazy val Special_strictEquals = getMemberMethod(SpecialPackageModule, newTermName("strictEquals"))
      lazy val Special_in = getMemberMethod(SpecialPackageModule, newTermName("in"))
      lazy val Special_instanceof = getMemberMethod(SpecialPackageModule, newTermName("instanceof"))
      lazy val Special_delete = getMemberMethod(SpecialPackageModule, newTermName("delete"))
      lazy val Special_forin = getMemberMethod(SpecialPackageModule, newTermName("forin"))
      lazy val Special_throw = getMemberMethod(SpecialPackageModule, newTermName("throw"))
      lazy val Special_tryCatch = getMemberMethod(SpecialPackageModule, newTermName("tryCatch"))
      lazy val Special_wrapAsThrowable = getMemberMethod(SpecialPackageModule, newTermName("wrapAsThrowable"))
      lazy val Special_unwrapFromThrowable = getMemberMethod(SpecialPackageModule, newTermName("unwrapFromThrowable"))
      lazy val Special_debugger = getMemberMethod(SpecialPackageModule, newTermName("debugger"))

    lazy val RuntimePackageModule = getPackageObject("scala.scalajs.runtime")
      lazy val Runtime_toScalaVarArgs             = getMemberMethod(RuntimePackageModule, newTermName("toScalaVarArgs"))
      lazy val Runtime_toJSVarArgs                = getMemberMethod(RuntimePackageModule, newTermName("toJSVarArgs"))
      lazy val Runtime_constructorOf              = getMemberMethod(RuntimePackageModule, newTermName("constructorOf"))
      lazy val Runtime_newConstructorTag          = getMemberMethod(RuntimePackageModule, newTermName("newConstructorTag"))
      lazy val Runtime_createInnerJSClass         = getMemberMethod(RuntimePackageModule, newTermName("createInnerJSClass"))
      lazy val Runtime_createLocalJSClass         = getMemberMethod(RuntimePackageModule, newTermName("createLocalJSClass"))
      lazy val Runtime_withContextualJSClassValue = getMemberMethod(RuntimePackageModule, newTermName("withContextualJSClassValue"))
      lazy val Runtime_privateFieldsSymbol        = getMemberMethod(RuntimePackageModule, newTermName("privateFieldsSymbol"))
      lazy val Runtime_linkingInfo                = getMemberMethod(RuntimePackageModule, newTermName("linkingInfo"))
      lazy val Runtime_identityHashCode           = getMemberMethod(RuntimePackageModule, newTermName("identityHashCode"))
      lazy val Runtime_dynamicImport              = getMemberMethod(RuntimePackageModule, newTermName("dynamicImport"))

    lazy val DynamicImportThunkClass = getRequiredClass("scala.scalajs.runtime.DynamicImportThunk")
      lazy val DynamicImportThunkClass_apply = getMemberMethod(DynamicImportThunkClass, nme.apply)

    lazy val LinkingInfoClass = getRequiredModule("scala.scalajs.LinkingInfo")
      lazy val LinkingInfoClass_linkTimeIf = getMemberMethod(LinkingInfoClass, newTermName("linkTimeIf"))

    lazy val Tuple2_apply = getMemberMethod(TupleClass(2).companionModule, nme.apply)

    // This is a def, since similar symbols (arrayUpdateMethod, etc.) are in runDefinitions
    // (rather than definitions) and we weren't sure if it is safe to make this a lazy val
    def ScalaRunTime_isArray: Symbol = getMemberMethod(ScalaRunTimeModule, newTermName("isArray")).suchThat(_.tpe.params.size == 2)

    lazy val ReflectModule = getRequiredModule("scala.scalajs.reflect.Reflect")
      lazy val Reflect_registerLoadableModuleClass = getMemberMethod(ReflectModule, newTermName("registerLoadableModuleClass"))
      lazy val Reflect_registerInstantiatableClass = getMemberMethod(ReflectModule, newTermName("registerInstantiatableClass"))

    lazy val EnableReflectiveInstantiationAnnotation = getRequiredClass("scala.scalajs.reflect.annotation.EnableReflectiveInstantiation")

    lazy val ExecutionContextModule = getRequiredModule("scala.concurrent.ExecutionContext")
      lazy val ExecutionContext_global = getMemberMethod(ExecutionContextModule, newTermName("global"))

    lazy val ExecutionContextImplicitsModule = getRequiredModule("scala.concurrent.ExecutionContext.Implicits")
      lazy val ExecutionContextImplicits_global = getMemberMethod(ExecutionContextImplicitsModule, newTermName("global"))
  }

  // scalastyle:on line.size.limit
}
