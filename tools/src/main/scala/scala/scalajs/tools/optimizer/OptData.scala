/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import scala.scalajs.tools.json._

object OptData {

  case class ClassInfoData(
      name: String,
      ancestorCount: Int,
      isStaticModule: Boolean,
      isInterface: Boolean,
      isImplClass: Boolean,
      isRawJSType: Boolean,
      encodedName: String,
      superClass: String,
      ancestors: List[String],
      isExported: Option[Boolean],
      methods: Map[String, MethodInfoData]
  )

  case class MethodInfoData(
      isAbstract: Option[Boolean] = None,
      isExported: Option[Boolean] = None,
      calledMethods: Option[Map[String, List[String]]] = None,
      calledMethodsStatic: Option[Map[String, List[String]]] = None,
      instantiatedClasses: Option[List[String]] = None,
      accessedModules: Option[List[String]] = None,
      accessedClassData: Option[List[String]] = None
  )

  object MethodInfoData {
    def placeholder(encodedName: String,
        isAbstract: Boolean = false): MethodInfoData = {
      MethodInfoData(
          isAbstract = if (isAbstract) Some(true) else None,
          isExported = None,
          calledMethods = None,
          calledMethodsStatic = None,
          instantiatedClasses = None,
          accessedModules = None,
          accessedClassData = None
      )
    }

    implicit object methodInfoDataToJSON extends JSONSerializer[MethodInfoData] {
      def serialize(x: MethodInfoData) = {
        new JSONObjBuilder()
          .opt("isAbstract",          x.isAbstract)
          .opt("isExported",          x.isExported)
          .opt("calledMethods",       x.calledMethods)
          .opt("calledMethodsStatic", x.calledMethodsStatic)
          .opt("instantiatedClasses", x.instantiatedClasses)
          .opt("accessedModules",     x.accessedModules)
          .opt("accessedClassData",   x.accessedClassData)
          .toJSON
      }
    }

    implicit object methodInfoDataFromJSON extends JSONDeserializer[MethodInfoData] {
      def deserialize(x: Object) = {
        val e = new JSONObjExtractor(x)
        MethodInfoData(
            e.opt[Boolean]("isAbstract"),
            e.opt[Boolean]("isExported"),
            e.opt[Map[String, List[String]]]("calledMethods"),
            e.opt[Map[String, List[String]]]("calledMethodsStatic"),
            e.opt[List[String]]("instantiatedClasses"),
            e.opt[List[String]]("accessedModules"),
            e.opt[List[String]]("accessedClassData")
        )
      }
    }
  }

  object ClassInfoData {
    def placeholder(encodedName: String): ClassInfoData = {
      val isStaticModule = encodedName endsWith "$"
      val isImplClass = encodedName endsWith "$class"
      ClassInfoData(
          name = s"<$encodedName>",
          ancestorCount = 0,
          isStaticModule = isStaticModule,
          isInterface = false, // assuming
          isImplClass = isImplClass,
          isRawJSType = false, // assuming
          encodedName = encodedName,
          superClass = if (isImplClass) "" else "java_lang_Object",
          ancestors = List(encodedName, "java_lang_Object"),
          isExported = None,
          methods = Map(
              "__init__" -> MethodInfoData.placeholder("__init__"),
              "init___" -> MethodInfoData.placeholder("init___"))
      )
    }

    implicit object classInfoDataToJSON extends JSONSerializer[ClassInfoData] {
      def serialize(x: ClassInfoData) = {
        new JSONObjBuilder()
          .fld("name",           x.name)
          .fld("ancestorCount",  x.ancestorCount)
          .fld("isStaticModule", x.isStaticModule)
          .fld("isInterface",    x.isInterface)
          .fld("isImplClass",    x.isImplClass)
          .fld("isRawJSType",    x.isRawJSType)
          .fld("encodedName",    x.encodedName)
          .fld("superClass",     x.superClass)
          .fld("ancestors",      x.ancestors)
          .opt("isExported",     x.isExported)
          .fld("methods",        x.methods)
          .toJSON
      }
    }

    implicit object classInfoDataFromJSON extends JSONDeserializer[ClassInfoData] {
      def deserialize(x: Object) = {
        val e = new JSONObjExtractor(x)
        ClassInfoData(
            e.fld[String]("name"),
            e.fld[Int]("ancestorCount"),
            e.fld[Boolean]("isStaticModule"),
            e.fld[Boolean]("isInterface"),
            e.fld[Boolean]("isImplClass"),
            e.fld[Boolean]("isRawJSType"),
            e.fld[String]("encodedName"),
            e.fld[String]("superClass"),
            e.fld[List[String]]("ancestors"),
            e.opt[Boolean]("isExported"),
            e.fld[Map[String, MethodInfoData]]("methods")
        )
      }
    }
  }
}
