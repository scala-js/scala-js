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

package org.scalajs.testinterface.internal

import scala.scalajs.js
import scala.scalajs.reflect.Reflect

import sbt.testing.Framework

private[internal] object FrameworkLoader {

  def loadFramework(frameworkName: String): Framework = {
    Reflect.lookupInstantiatableClass(frameworkName).fold[Framework] {
      val exportsNamespace =
        scala.scalajs.runtime.environmentInfo.exportsNamespace
      val parts = frameworkName.split('.')
      val ctor = parts.foldLeft(exportsNamespace)(_.selectDynamic(_))
      js.Dynamic.newInstance(ctor)().asInstanceOf[Framework]
    } { clazz =>
      clazz.newInstance().asInstanceOf[Framework]
    }
  }

  def detectFrameworkNames(names: List[List[String]]): List[Option[String]] = {
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

    for (frameworkNames <- names)
      yield frameworkNames.find(frameworkExists(_))
  }
}
