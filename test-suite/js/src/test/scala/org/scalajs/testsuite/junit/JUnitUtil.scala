package org.scalajs.testsuite.junit

import org.scalajs.junit.JUnitTestBootstrapper
import org.junit.Assert.fail

import scalajs.js

object JUnitUtil {
  private final val BootstrapperSuffix = "$scalajs$junit$bootstrapper"

  def loadBootstrapper(classFullName: String): JUnitTestBootstrapper = {
    val fullName = s"$classFullName$BootstrapperSuffix"
    try {
      val exportsNamespace =
        scala.scalajs.runtime.environmentInfo.exportsNamespace
      fullName.split('.').foldLeft(exportsNamespace) { (obj, n) =>
        obj.selectDynamic(n)
      }.apply().asInstanceOf[JUnitTestBootstrapper]
    } catch {
      case ex: Throwable =>
        throw new AssertionError(s"could not load $fullName: ${ex.getMessage}")
    }
  }
}
