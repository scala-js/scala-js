package org.scalajs.testinterface.internal

import scala.scalajs.js

import sbt.testing.Framework

private[internal] object FrameworkLoader {

  def loadFramework(frameworkName: String): Framework = {
    val parts = frameworkName.split('.')
    val ctor = parts.foldLeft(js.Dynamic.global)(_.selectDynamic(_))
    js.Dynamic.newInstance(ctor)().asInstanceOf[Framework]
  }

}
