package org.scalajs.testinterface

import scala.scalajs.js
import scala.scalajs.reflect.Reflect

import sbt.testing.Framework

private[testinterface] object FrameworkLoader {

  def loadFramework(frameworkName: String): Framework = {
    val clazz = Reflect.lookupInstantiatableClass(frameworkName).getOrElse {
      throw new InstantiationError(frameworkName)
    }
    clazz.newInstance().asInstanceOf[Framework]
  }

  def detectFrameworkNames(names: List[List[String]]): List[Option[String]] = {
    def frameworkExists(name: String): Boolean = {
      Reflect.lookupInstantiatableClass(name).exists { clazz =>
        classOf[sbt.testing.Framework].isAssignableFrom(clazz.runtimeClass)
      }
    }

    for (frameworkNames <- names)
      yield frameworkNames.find(frameworkExists(_))
  }

  def tryLoadFramework(names: List[String]): Option[Framework] = {
    def tryLoad(name: String): Option[Framework] = {
      Reflect.lookupInstantiatableClass(name).collect {
        case clazz if classOf[Framework].isAssignableFrom(clazz.runtimeClass) =>
          clazz.newInstance().asInstanceOf[Framework]
      }
    }

    names.toStream.map(tryLoad).flatten.headOption
  }
}
