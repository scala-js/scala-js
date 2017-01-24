package org.scalajs.testsuite.junit

import org.scalajs.junit.JUnitTestBootstrapper
import org.junit.Assert.fail

import org.scalajs.testinterface._

object JUnitUtil {
  private final val BootstrapperSuffix = "$scalajs$junit$bootstrapper"

  def loadBootstrapper(classFullName: String): JUnitTestBootstrapper = {
    val fullName = s"$classFullName$BootstrapperSuffix"
    try {
      val loader = new ScalaJSClassLoader(
          scala.scalajs.runtime.environmentInfo.exportsNamespace)
      TestUtils.loadModule(fullName, loader).asInstanceOf[JUnitTestBootstrapper]
    } catch {
      case ex: Throwable =>
        throw new AssertionError(s"could not load $fullName: ${ex.getMessage}")
    }
  }
}
