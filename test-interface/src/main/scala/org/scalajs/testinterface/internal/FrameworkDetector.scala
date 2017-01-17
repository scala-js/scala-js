package org.scalajs.testinterface.internal

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSConverters._
import scala.scalajs.reflect.Reflect

private[internal] object FrameworkDetector {
  @JSExportTopLevel("org.scalajs.testinterface.internal.detectFrameworks")
  def detectFrameworks(
      frameworksData: js.Array[js.Array[String]]): js.Array[js.UndefOr[String]] = {

    def frameworkExistsInReflect(name: String): Boolean = {
      Reflect.lookupInstantiatableClass(name).exists { clazz =>
        classOf[sbt.testing.Framework].isAssignableFrom(clazz.runtimeClass)
      }
    }

    def frameworkExistsInExportsNamespace(name: String): Boolean = {
      /* This happens for testing frameworks developed before 0.6.15 that have
       * not yet updated to using reflective instantiation, and are still
       * using exports.
       * Note that here, we have to assume that whatever we find is indeed a
       * proper class export for a class extending sbt.testing.Framework.
       */
      val exportsNamespace =
        scala.scalajs.runtime.environmentInfo.exportsNamespace
      name.split('.').foldLeft[js.UndefOr[js.Dynamic]](exportsNamespace) {
        (prev, part) => prev.map(_.selectDynamic(part))
      }.isDefined
    }

    def frameworkExists(name: String): Boolean =
      frameworkExistsInReflect(name) || frameworkExistsInExportsNamespace(name)

    for (frameworkNames <- frameworksData)
      yield frameworkNames.find(frameworkExists(_)).orUndefined
  }
}
