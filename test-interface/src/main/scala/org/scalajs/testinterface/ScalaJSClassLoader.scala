package org.scalajs.testinterface

import scala.scalajs.js

import java.net.URL
import java.io.InputStream
import java.util.Enumeration

/** DEPRECATED A dummy [[java.lang.ClassLoader]] that allows to store a
 *  JavaScript object against which classes are resolved.
 *
 *  The only reason it extends [[java.lang.ClassLoader]] is typing.
 *
 *  @note
 *    `ScalaJSClassLoader` is deprecated, although it is not annotated with
 *    `@deprecated` for internal reasons. Use the reflection API in
 *    [[scala.scalajs.reflect.Reflect]] instead of matching against
 *    `ScalaJSClassLoader`.
 */
final class ScalaJSClassLoader(
    @deprecated(
        "Use scala.scalajs.reflect.Reflect instead of ScalaJSClassLoader.",
        "0.6.25")
    val namespace: js.Dynamic) extends ClassLoader(null) {

  private def nimp: Nothing = {
    throw new NotImplementedError(
        "A ScalaJSClassLoader is a dummy. " +
        "Use scala.scalajs.reflect.Reflect (JS-only) or " +
        "https://github.com/portable-scala/portable-scala-reflect (portable) " +
        "to instantiate things.")
  }

  override def clearAssertionStatus(): Unit = nimp
  override def getResource(name: String): URL = nimp
  override def getResourceAsStream(name: String): InputStream = nimp
  override def getResources(name: String): Enumeration[URL] = nimp
  override def loadClass(name: String): Class[_] = nimp
  override def setClassAssertionStatus(className: String, enabled: Boolean): Unit = nimp
  override def setDefaultAssertionStatus(enabled: Boolean): Unit = nimp
  override def setPackageAssertionStatus(packageName: String, enabled: Boolean): Unit = nimp
}
