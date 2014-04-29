/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.language.implicitConversions

import scala.collection.mutable
import scala.tools.nsc._

import java.io.{ File, PrintWriter, BufferedOutputStream, FileOutputStream }

import scala.scalajs.ir
import ir.{Trees => js, ClassKind}
import ir.Infos._

trait ClassInfos extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  class ClassInfoBuilder(val symbol: ClassSymbol) {
    val name = classNameOf(symbol)
    val encodedName = encodeClassFullName(symbol)
    var isExported: Boolean = false
    val ancestorCount = symbol.ancestors.count(!_.isInterface)
    val kind = {
      if (isStaticModule(symbol))            ClassKind.ModuleClass
      else if (symbol.isInterface)           ClassKind.Interface
      else if (isRawJSType(symbol.tpe))      ClassKind.RawJSType
      else if (isHijackedBoxedClass(symbol)) ClassKind.HijackedClass
      else if (symbol.isImplClass)           ClassKind.TraitImpl
      else                                   ClassKind.Class
    }
    val superClass =
      if (kind.isClass || kind == ClassKind.HijackedClass)
        encodeClassFullName(symbol.superClass)
      else
        ""
    val ancestors = (symbol :: symbol.ancestors) map encodeClassFullName

    val methodInfos = mutable.ListBuffer.empty[MethodInfoBuilder]

    def addMethod(encodedName: String, isAbstract: Boolean = false,
        isExported: Boolean = false): MethodInfoBuilder = {
      val b = new MethodInfoBuilder(encodedName, isAbstract, isExported)
      methodInfos += b
      b
    }

    def result(): ClassInfo = {
      ClassInfo(name, encodedName, isExported, ancestorCount, kind,
          superClass, ancestors, methodInfos.map(_.result()).result())
    }
  }

  class MethodInfoBuilder(val encodedName: String,
      val isAbstract: Boolean = false,
      val isExported: Boolean = false) {

    val calledMethods = mutable.Set.empty[(String, String)] // (tpe, method)
    val calledMethodsStatic = mutable.Set.empty[(String, String)] // (class, method)
    val instantiatedClasses = mutable.Set.empty[String]
    val accessedModules = mutable.Set.empty[String]
    val accessedClassData = mutable.Set.empty[String]
    var optimizerHints: OptimizerHints = OptimizerHints.empty

    def callsMethod(ownerIdent: js.Ident, method: js.Ident): Unit =
      calledMethods += ((patchClassName(ownerIdent.name), method.name))

    def callsMethod(owner: Symbol, method: js.Ident): Unit =
      calledMethods += ((patchClassName(encodeClassFullName(owner)), method.name))

    def callsMethodStatic(ownerIdent: js.Ident, method: js.Ident): Unit =
      calledMethodsStatic += ((patchClassName(ownerIdent.name), method.name))

    def instantiatesClass(classSym: Symbol): Unit =
      instantiatedClasses += patchClassName(encodeClassFullName(classSym))

    def accessesModule(moduleClassSym: Symbol): Unit =
      accessedModules += patchModuleName(encodeModuleFullName(moduleClassSym))

    def accessesClassData(classSym: Symbol): Unit =
      if (!classSym.isPrimitiveValueClass)
        accessedClassData += encodeClassFullName(classSym)

    def createsAnonFunction(funInfo: ClassInfoBuilder): Unit = {
      for (methodInfo <- funInfo.methodInfos) {
        calledMethods ++= methodInfo.calledMethods
        calledMethodsStatic ++= methodInfo.calledMethodsStatic
        instantiatedClasses ++= methodInfo.instantiatedClasses
        accessedModules ++= methodInfo.accessedModules
        accessedClassData ++= methodInfo.accessedClassData
      }
    }

    private def patchClassName(name: String): String = name match {
      case "java_lang_String$" => "scala_scalajs_runtime_RuntimeString$"
      case _ => name
    }

    private def patchModuleName(name: String): String = name match {
      case "java_lang_String" => "scala_scalajs_runtime_RuntimeString"
      case _ => name
    }

    def result(): MethodInfo = {
      MethodInfo(
          encodedName,
          isAbstract,
          isExported,
          calledMethods.toList.groupBy(_._1).mapValues(_.map(_._2)),
          calledMethodsStatic.toList.groupBy(_._1).mapValues(_.map(_._2)),
          instantiatedClasses.toList,
          accessedModules.result.toList,
          accessedClassData.result.toList,
          optimizerHints
      )
    }
  }

  private def classNameOf(sym: Symbol): String =
    if (needsModuleClassSuffix(sym)) sym.fullName + nme.MODULE_SUFFIX_STRING
    else sym.fullName
}
