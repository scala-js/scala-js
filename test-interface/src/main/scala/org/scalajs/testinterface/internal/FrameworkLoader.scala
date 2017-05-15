package org.scalajs.testinterface.internal

import scala.scalajs.js
import scala.scalajs.reflect.Reflect

import sbt.testing.Framework

private[internal] object FrameworkLoader {

  def loadFramework(frameworkName: String): Framework = {
    val clazz = Reflect.lookupInstantiatableClass(frameworkName).getOrElse {
      throw new InstantiationError(frameworkName)
    }
    clazz.newInstance().asInstanceOf[Framework]
  }

}
