/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.core.compiler

import scala.language.implicitConversions

import scala.collection.mutable
import scala.tools.nsc._

import java.io.{ File, PrintWriter, BufferedOutputStream, FileOutputStream }

import org.scalajs.core.ir
import ir.{Trees => js, Types => jstpe, ClassKind}
import ir.Infos._

trait ClassInfos extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  /** Class data that are never eliminated by dce, so we don't need to
   *  record them.
   */
  private val AlwaysPresentClassData = {
    import ir.Definitions._
    Set("V", "Z", "C", "B", "S", "I", "J", "F", "D",
        ObjectClass, StringClass)
  }

  class ClassInfoBuilder(val symbol: ClassSymbol) {
    val encodedName = encodeClassFullName(symbol)
    var isExported: Boolean = false

    val kind = {
      if (isRawJSType(symbol.tpe))           ClassKind.RawJSType
      else if (isStaticModule(symbol))       ClassKind.ModuleClass
      else if (symbol.isInterface)           ClassKind.Interface
      else if (isHijackedBoxedClass(symbol)) ClassKind.HijackedClass
      else                                   ClassKind.Class
    }

    val superClass =
      if (symbol.isInterface) None
      else Some(encodeClassFullName(symbol.superClass))

    val interfaces = for {
      parent <- symbol.info.parents
      typeSym = parent.typeSymbol
      _ = assert(typeSym != NoSymbol, "parent needs symbol")
      if (typeSym.isInterface)
    } yield {
      encodeClassFullName(typeSym)
    }

    val methodInfos = mutable.ListBuffer.empty[MethodInfoBuilder]

    def addMethod(encodedName: String, isStatic: Boolean,
        isAbstract: Boolean = false,
        isExported: Boolean = false): MethodInfoBuilder = {
      val b = new MethodInfoBuilder(encodedName, isStatic, isAbstract, isExported)
      methodInfos += b
      b
    }

    def result(): ClassInfo = {
      ClassInfo(encodedName, isExported, kind,
          superClass, interfaces, methodInfos.map(_.result()).result())
    }
  }

  class MethodInfoBuilder(val encodedName: String,
      val isStatic: Boolean,
      val isAbstract: Boolean = false,
      val isExported: Boolean = false) {

    val methodsCalled = mutable.Set.empty[(String, String)] // (tpe, method)
    val methodsCalledStatically = mutable.Set.empty[(String, String)] // (class, method)
    val staticMethodsCalled = mutable.Set.empty[(String, String)] // (tpe, method)
    val instantiatedClasses = mutable.Set.empty[String]
    val accessedModules = mutable.Set.empty[String]
    val accessedClassData = mutable.Set.empty[String]

    def callsMethod(ownerIdent: js.Ident, method: js.Ident): Unit =
      methodsCalled += ((patchClassName(ownerIdent.name), method.name))

    def callsMethod(owner: Symbol, method: js.Ident): Unit =
      methodsCalled += ((patchClassName(encodeClassFullName(owner)), method.name))

    def callsMethodStatically(ownerIdent: js.Ident, method: js.Ident): Unit =
      methodsCalledStatically += ((patchClassName(ownerIdent.name), method.name))

    def callsMethodStatically(owner: Symbol, method: js.Ident): Unit =
      methodsCalledStatically += ((patchClassName(encodeClassFullName(owner)), method.name))

    def callsStaticMethod(ownerIdent: js.Ident, method: js.Ident): Unit =
      staticMethodsCalled += ((patchClassName(ownerIdent.name), method.name))

    def instantiatesClass(classSym: Symbol): Unit =
      instantiatedClasses += patchClassName(encodeClassFullName(classSym))

    def accessesModule(moduleClassSym: Symbol): Unit =
      accessedModules += patchClassName(encodeClassFullName(moduleClassSym))

    def accessesClassData(refType: jstpe.ReferenceType): Unit = {
      val className = refType match {
        case jstpe.ClassType(name)    => name
        case jstpe.ArrayType(base, _) => base
      }
      if (!AlwaysPresentClassData.contains(className))
        accessedClassData += className
    }

    def createsAnonFunction(funInfo: ClassInfoBuilder): Unit = {
      for (methodInfo <- funInfo.methodInfos) {
        methodsCalled ++= methodInfo.methodsCalled
        methodsCalledStatically ++= methodInfo.methodsCalledStatically
        staticMethodsCalled ++= methodInfo.staticMethodsCalled
        instantiatedClasses ++= methodInfo.instantiatedClasses
        accessedModules ++= methodInfo.accessedModules
        accessedClassData ++= methodInfo.accessedClassData
      }
    }

    private def patchClassName(name: String): String = name match {
      case "jl_String$" => "sjsr_RuntimeString$"
      case _ => name
    }

    def result(): MethodInfo = {
      MethodInfo(
          encodedName,
          isStatic,
          isAbstract,
          isExported,
          methodsCalled.toList.groupBy(_._1).mapValues(_.map(_._2)),
          methodsCalledStatically.toList.groupBy(_._1).mapValues(_.map(_._2)),
          staticMethodsCalled.toList.groupBy(_._1).mapValues(_.map(_._2)),
          instantiatedClasses.toList,
          accessedModules.result.toList,
          accessedClassData.result.toList
      )
    }
  }

  private def classNameOf(sym: Symbol): String =
    if (needsModuleClassSuffix(sym)) sym.fullName + nme.MODULE_SUFFIX_STRING
    else sym.fullName
}
