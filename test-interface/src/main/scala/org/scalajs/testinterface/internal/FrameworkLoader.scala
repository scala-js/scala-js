package org.scalajs.testinterface.internal

import scala.scalajs.js

import sbt.testing.Framework

private[internal] object FrameworkLoader {

  def loadFramework(frameworkName: String): Framework = {
    val exportsNamespace =
      scala.scalajs.runtime.environmentInfo.exportsNamespace
    val parts = frameworkName.split('.')
    val ctor = parts.foldLeft(exportsNamespace)(_.selectDynamic(_))
    js.Dynamic.newInstance(ctor)().asInstanceOf[Framework]
  }

}
