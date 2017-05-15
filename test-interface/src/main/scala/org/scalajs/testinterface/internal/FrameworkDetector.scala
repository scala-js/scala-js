package org.scalajs.testinterface.internal

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSConverters._
import scala.scalajs.reflect.Reflect

private[internal] object FrameworkDetector {
  @JSExportTopLevel("org.scalajs.testinterface.internal.detectFrameworks")
  def detectFrameworks(
      frameworksData: js.Array[js.Array[String]]): js.Array[js.UndefOr[String]] = {

    def frameworkExists(name: String): Boolean = {
      Reflect.lookupInstantiatableClass(name).exists { clazz =>
        classOf[sbt.testing.Framework].isAssignableFrom(clazz.runtimeClass)
      }
    }

    for (frameworkNames <- frameworksData)
      yield frameworkNames.find(frameworkExists(_)).orUndefined
  }
}
