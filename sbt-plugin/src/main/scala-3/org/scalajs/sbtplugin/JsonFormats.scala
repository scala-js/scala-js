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

package org.scalajs.sbtplugin

import sjsonnew._
import sjsonnew.BasicJsonProtocol._

import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._

trait JsonFormats {

  /** Hand-written JsonFormat for ModuleInitializer.
   *
   *  We hand-write JsonFormat[ModuleInitializer] instead of using sjson-new's
   *  auto-derivation because ModuleInitializer internally uses `MethodName`
   *  (which contains `List[TypeRef]`) for parameter and return types.
   *  Serializing the full `MethodName` would require JsonFormat instances for
   *  the IR type hierarchy (`TypeRef`, `ClassName`, etc.).
   *
   *  However, for ModuleInitializer, we don't need the full `MethodName`,
   *  because MainMethod always has a fixed signature
   *  (`() -> Unit or Array[String] -> Unit`). We only need to serialize the
   *  simple method name string (e.g., "main"), and reconstruct
   *  `ModuleInitializer`s using factory methods like `mainMethod`.
   */
  implicit val moduleInitializerJsonFormat: JsonFormat[ModuleInitializer] = {
    new JsonFormat[ModuleInitializer] {
      def write[J](x: ModuleInitializer, builder: Builder[J]): Unit = {
        builder.beginObject()
        builder.addField("moduleID", x.moduleID)
        ModuleInitializerImpl.fromInitializer(x.initializer) match {
          case VoidMainMethod(className, methodName) =>
            builder.addField("type", "VoidMainMethod")
            builder.addField("className", className.nameString)
            builder.addField("mainMethodName", methodName.simpleName.nameString)
          case MainMethodWithArgs(className, methodName, args) =>
            builder.addField("type", "MainMethodWithArgs")
            builder.addField("className", className.nameString)
            builder.addField("mainMethodName", methodName.simpleName.nameString)
            builder.addField("args", args)
        }
        builder.endObject()
      }

      def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): ModuleInitializer = {
        jsOpt match {
          case Some(js) =>
            unbuilder.beginObject(js)
            val moduleID = unbuilder.readField[String]("moduleID")
            val tpe = unbuilder.readField[String]("type")
            val className = unbuilder.readField[String]("className")
            val mainMethodName = unbuilder.readField[String]("mainMethodName")
            val base = tpe match {
              case "VoidMainMethod" =>
                ModuleInitializer.mainMethod(className, mainMethodName)
              case "MainMethodWithArgs" =>
                val args = unbuilder.readField[List[String]]("args")
                ModuleInitializer.mainMethodWithArgs(className, mainMethodName, args)
            }
            unbuilder.endObject()
            base.withModuleID(moduleID)
          case None =>
            deserializationError("Expected ModuleInitializer")
        }
      }
    }
  }
}
