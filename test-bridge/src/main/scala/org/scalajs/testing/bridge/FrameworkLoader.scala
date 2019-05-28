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

package org.scalajs.testing.bridge

import scala.scalajs.js
import scala.scalajs.reflect.Reflect

import sbt.testing.Framework

private[bridge] object FrameworkLoader {

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

    names.iterator.map(tryLoad).collectFirst {
      case Some(framework) => framework
    }
  }
}
