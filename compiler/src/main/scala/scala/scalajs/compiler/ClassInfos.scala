/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.language.implicitConversions

import scala.collection.mutable
import scala.tools.nsc._

import java.io.{ File, PrintWriter, BufferedOutputStream, FileOutputStream }

trait ClassInfos extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  /* This component uses our js.Tree API to build JSON data to be output in
   * the .sjsinfo file. The JSONBuilder object below provides helpers to do so.
   */
  import JSONBuilder._

  class ClassInfoBuilder(val symbol: ClassSymbol) {
    val isStaticModule = ClassInfos.this.isStaticModule(symbol)
    val isInterface = symbol.isInterface
    val isImplClass = symbol.isImplClass
    val isRawJSType = ClassInfos.this.isRawJSType(symbol.tpe)
    val name = classNameOf(symbol)
    val ancestorCount = symbol.ancestors.count(!_.isInterface)
    val encodedName = encodeClassFullName(symbol)
    val superClass =
      if (!isInterface && !isImplClass) encodeClassFullName(symbol.superClass)
      else ""
    val ancestors = (symbol :: symbol.ancestors) map encodeClassFullName

    var isExported: Boolean = false
    val methodInfos = mutable.ListBuffer.empty[MethodInfoBuilder]

    def addMethod(ident: js.Ident, isAbstract: Boolean = false,
        isExported: Boolean = false): MethodInfoBuilder = {
      val b = new MethodInfoBuilder(ident, isAbstract, isExported)
      methodInfos += b
      b
    }

    def toJSON: js.Tree = {
      obj(
          "name" -> name,
          "ancestorCount" -> ancestorCount,
          "isStaticModule" -> isStaticModule,
          "isInterface" -> isInterface,
          "isImplClass" -> isImplClass,
          "isRawJSType" -> isRawJSType,
          "encodedName" -> encodedName,
          "superClass" -> superClass,
          "ancestors" -> ancestors,
          "isExported" -> isExported,
          "methods" -> obj(methodInfos.map(_.toJSONPair): _*)
      )
    }
  }

  class MethodInfoBuilder(val ident: js.Ident, val isAbstract: Boolean = false,
      val isExported: Boolean = false) {
    val encodedName = ident.name

    val calledMethods = mutable.Set.empty[(String, String)] // (class, method)
    val instantiatedClasses = mutable.Set.empty[String]
    val accessedModules = mutable.Set.empty[String]
    val accessedClassData = mutable.Set.empty[String]

    def callsMethod(ownerIdent: js.Ident, method: js.Ident): Unit =
      calledMethods += ((ownerIdent.name, method.name))

    def callsMethod(owner: Symbol, method: js.Ident): Unit =
      calledMethods += ((encodeClassFullName(owner), method.name))

    def instantiatesClass(classSym: Symbol): Unit =
      instantiatedClasses += encodeClassFullName(classSym)

    def accessesModule(moduleClassSym: Symbol): Unit =
      accessedModules += encodeModuleFullName(moduleClassSym)

    def accessesClassData(classSym: Symbol): Unit =
      if (!classSym.isPrimitiveValueClass)
        accessedClassData += encodeClassFullName(classSym)

    def createsAnonFunction(funInfo: ClassInfoBuilder): Unit = {
      for (methodInfo <- funInfo.methodInfos) {
        calledMethods ++= methodInfo.calledMethods
        instantiatedClasses ++= methodInfo.instantiatedClasses
        accessedModules ++= methodInfo.accessedModules
        accessedClassData ++= methodInfo.accessedClassData
      }
    }

    def toJSONPair: (String, js.Tree) = {
      val fields = mutable.ListBuffer.empty[(String, js.Tree)]
      if (isAbstract)
        fields += "isAbstract" -> true
      if (isExported)
        fields += "isExported" -> true
      if (calledMethods.nonEmpty) {
        val groupedByType = calledMethods.toList.groupBy(_._1)
        fields += ("calledMethods" ->
            obj(groupedByType.mapValues(_.map(_._2): js.Tree).toList: _*))
      }
      if (instantiatedClasses.nonEmpty)
        fields += "instantiatedClasses" -> instantiatedClasses.toList
      if (accessedModules.nonEmpty)
        fields += "accessedModules" -> accessedModules.toList
      if (accessedClassData.nonEmpty)
        fields += "accessedClassData" -> accessedClassData.toList
      (encodedName, obj(fields: _*))
    }
  }

  private def classNameOf(sym: Symbol): String =
    if (needsModuleClassSuffix(sym)) sym.fullName + nme.MODULE_SUFFIX_STRING
    else sym.fullName

  /** Helper methods and implicits to build js.Trees in a JSON way. */
  private object JSONBuilder {
    /* JSON trees do not have/need positions, since we don't emit source maps
     * for them. Since all js.Tree constructors require their position as an
     * implicit argument, we put NoPosition in the implicit scope for JSON
     * building.
     */
    implicit val dummyPos: Position = NoPosition

    /** Object construction. */
    def obj(fields: (String, js.Tree)*): js.Tree =
      js.ObjectConstr(fields.map(f => (js.StringLiteral(f._1), f._2)).toList)

    implicit def string2lit(s: String): js.StringLiteral =
      js.StringLiteral(s)

    implicit def int2lit(i: Int): js.IntLiteral =
      js.IntLiteral(i)

    implicit def bool2lit(b: Boolean): js.BooleanLiteral =
      js.BooleanLiteral(b)

    implicit def seq2array[A](seq: Seq[A])(
        implicit convA: A => js.Tree): js.ArrayConstr =
      js.ArrayConstr(seq.map(convA).toList)

    implicit def pair2array[A, B](pair: (A, B))(
        implicit convA: A => js.Tree, convB: B => js.Tree): js.ArrayConstr =
      js.ArrayConstr(List(convA(pair._1), convB(pair._2)))
  }
}
