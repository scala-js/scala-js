package org.scalajs.testinterface

import scala.scalajs.js

import java.net.URL
import java.io.InputStream
import java.util.Enumeration

/** A dummy [[java.lang.ClassLoader]] that allows to store a JavaScript object
 *  against which classes are resolved. The only reason it extends
 *  [[java.lang.ClassLoader]] is typing.
 */
final class ScalaJSClassLoader(
    val namespace: js.Dynamic) extends ClassLoader(null) {

  private def nimp: Nothing =
    throw new NotImplementedError("A ScalaJSClassLoader is a dummy. " +
        "Use scala.scalajs.testinterface.TestUtils to instantiate things.")

  override def clearAssertionStatus(): Unit = nimp
  override def getResource(name: String): URL = nimp
  override def getResourceAsStream(name: String): InputStream = nimp
  override def getResources(name: String): Enumeration[URL] = nimp
  override def loadClass(name: String): Class[_] = nimp
  override def setClassAssertionStatus(className: String, enabled: Boolean): Unit = nimp
  override def setDefaultAssertionStatus(enabled: Boolean): Unit = nimp
  override def setPackageAssertionStatus(packageName: String, enabled: Boolean): Unit = nimp
}
