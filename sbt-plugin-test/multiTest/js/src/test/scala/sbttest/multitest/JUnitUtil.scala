package sbttest.multitest

import org.scalajs.junit.JUnitTestBootstrapper
import org.junit.Assert.fail

import scala.scalajs.reflect.Reflect

object JUnitUtil {
  private final val bootstrapperSuffix = "$scalajs$junit$bootstrapper"

  def loadBootstrapper(classFullName: String): JUnitTestBootstrapper = {
    val fullName = s"$classFullName$bootstrapperSuffix"
    try {
      val modClass = Reflect.lookupLoadableModuleClass(fullName + "$").get
      modClass.loadModule().asInstanceOf[JUnitTestBootstrapper]
    } catch {
      case ex: Throwable =>
        throw new AssertionError(s"could not load $fullName: ${ex.getMessage}")
    }
  }
}
