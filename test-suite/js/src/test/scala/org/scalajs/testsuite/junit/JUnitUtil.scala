package org.scalajs.testsuite.junit

import org.scalajs.junit.JUnitTestBootstrapper
import org.junit.Assert.fail

import scalajs.js

object JUnitUtil {
  private final val BootstrapperSuffix = "$scalajs$junit$bootstrapper"

  def loadBootstrapper(classFullName: String): JUnitTestBootstrapper = {
    val fullName = s"$classFullName$BootstrapperSuffix"
    try {
      fullName.split('.').foldLeft(js.Dynamic.global) { (obj, n) =>
        obj.selectDynamic(n)
      }.apply().asInstanceOf[JUnitTestBootstrapper]
    } catch {
      case ex: Throwable =>
        throw new AssertionError(s"could not load $fullName: ${ex.getMessage}")
    }
  }
}
