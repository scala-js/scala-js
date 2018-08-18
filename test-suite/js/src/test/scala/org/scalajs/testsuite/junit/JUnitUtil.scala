package org.scalajs.testsuite.junit

import org.scalajs.junit.JUnitTestBootstrapper
import org.junit.Assert.fail

import scala.scalajs.reflect.Reflect

object JUnitUtil {
  private final val BootstrapperSuffix = "$scalajs$junit$bootstrapper"

  def loadBootstrapper(classFullName: String): JUnitTestBootstrapper = {
    val fullName = s"$classFullName$BootstrapperSuffix"
    try {
      Reflect
        .lookupLoadableModuleClass(fullName + "$")
        .getOrElse(throw new ClassNotFoundException(s"Cannot find $fullName$$"))
        .loadModule()
        .asInstanceOf[JUnitTestBootstrapper]
    } catch {
      case ex: Throwable =>
        throw new AssertionError(s"could not load $fullName: ${ex.getMessage}")
    }
  }
}
