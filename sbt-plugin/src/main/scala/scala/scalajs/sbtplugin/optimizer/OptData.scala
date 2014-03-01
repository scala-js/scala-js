package scala.scalajs.sbtplugin.optimizer

import sbt._
import net.liftweb.json._

object OptData {
  private implicit val formats = DefaultFormats

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
  }

  case class MethodInfoData(
      isAbstract: Option[Boolean],
      isExported: Option[Boolean],
      calledMethods: Option[Map[String, List[String]]],
      instantiatedClasses: Option[List[String]],
      accessedModules: Option[List[String]],
      accessedClassData: Option[List[String]]
  )

  object MethodInfoData {
    def placeholder(encodedName: String,
        isAbstract: Boolean = false): MethodInfoData = {
      MethodInfoData(
          isAbstract = if (isAbstract) Some(true) else None,
          isExported = None,
          calledMethods = None,
          instantiatedClasses = None,
          accessedModules = None,
          accessedClassData = None
      )
    }
  }

  def readData(infoFile: File): ClassInfoData =
    Extraction.extract[ClassInfoData](parse(IO.read(infoFile)))
}
