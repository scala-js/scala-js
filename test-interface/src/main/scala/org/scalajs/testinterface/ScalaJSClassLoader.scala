package org.scalajs.testinterface

import scala.scalajs.js

import java.net.URL
import java.io.InputStream
import java.util.Enumeration

/** A dummy [[java.lang.ClassLoader]] for Scala.js testing frameworks.
 *
 *  This class loader does *not* implement the contract of
 *  [[java.lang.ClassLoader]]. It is exclusively used as a dummy class loader
 *  to preserve source compatibility with the sbt testing interface.
 */
final class ScalaJSClassLoader extends ClassLoader(null) {

  @deprecated(
      "The `namespace` argument is ignored. " +
      "Use the constructor with 0 argument.",
      "1.0.0")
  def this(namespace: js.Dynamic) = this()

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
