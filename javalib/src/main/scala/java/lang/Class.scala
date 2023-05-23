/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.lang

import java.lang.constant.Constable
import java.io.Serializable

import scala.scalajs.js

@js.native
private trait ScalaJSClassData[A] extends js.Object {
  val name: String = js.native
  val isPrimitive: scala.Boolean = js.native
  val isInterface: scala.Boolean = js.native
  val isArrayClass: scala.Boolean = js.native

  def isInstance(obj: Any): scala.Boolean = js.native
  def isAssignableFrom(that: ScalaJSClassData[_]): scala.Boolean = js.native
  def checkCast(obj: Any): scala.Unit = js.native

  def getSuperclass(): Class[_ >: A] = js.native
  def getComponentType(): Class[_] = js.native

  def newArrayOfThisClass(dimensions: js.Array[Int]): AnyRef = js.native
}

final class Class[A] private (data0: Object)
    extends Object with Serializable with Constable {

  private[this] val data: ScalaJSClassData[A] =
    data0.asInstanceOf[ScalaJSClassData[A]]

  private[this] var cachedSimpleName: String = _

  /** Access to `data` for other instances or `@inline` methods.
   *
   *  Directly accessing the `data` field from `@inline` methods will cause
   *  scalac to make the field public and mangle its name. Since the Emitter
   *  relies on the field being called exactly `data` in some of its
   *  optimizations, we must avoid that.
   *
   *  This non-`@noinline` method can be used to access the field without
   *  triggering scalac's mangling. Since it is a trivial accessor, the
   *  Scala.js optimizer will inline it anyway.
   */
  private def getData(): ScalaJSClassData[A] = data

  override def toString(): String = {
    (if (isInterface()) "interface " else
        if (isPrimitive()) "" else "class ")+getName()
  }

  def isInstance(obj: Any): scala.Boolean =
    data.isInstance(obj)

  def isAssignableFrom(that: Class[_]): scala.Boolean =
    this.data.isAssignableFrom(that.getData())

  def isInterface(): scala.Boolean =
    data.isInterface

  def isArray(): scala.Boolean =
    data.isArrayClass

  def isPrimitive(): scala.Boolean =
    data.isPrimitive

  def getName(): String =
    data.name

  def getSimpleName(): String = {
    if (cachedSimpleName == null)
      cachedSimpleName = computeCachedSimpleNameBestEffort()
    cachedSimpleName
  }

  /** Computes a best-effort guess of what `getSimpleName()` should return.
   *
   *  The JavaDoc says:
   *
   *  > Returns the simple name of the underlying class as given in the source
   *  > code. Returns an empty string if the underlying class is anonymous.
   *  >
   *  > The simple name of an array is the simple name of the component type
   *  > with "[]" appended. In particular the simple name of an array whose
   *  > component type is anonymous is "[]".
   *
   *  Note the "as given in the source code" part. Clearly, this is not always
   *  the case, since Scala local classes receive a numeric suffix, for
   *  example.
   *
   *  In the absence of precise algorithm, we make a best-effort to make
   *  reasonable use cases mimic the JVM.
   */
  private def computeCachedSimpleNameBestEffort(): String = {
    @inline def isDigit(c: Char): scala.Boolean = c >= '0' && c <= '9'

    if (isArray()) {
      getComponentType().getSimpleName() + "[]"
    } else {
      val name = data.name
      var idx = name.length - 1

      // Include trailing '$'s for module class names
      while (idx >= 0 && name.charAt(idx) == '$') {
        idx -= 1
      }

      // Include '$'s followed by '0-9's for local class names
      if (idx >= 0 && isDigit(name.charAt(idx))) {
        idx -= 1
        while (idx >= 0 && isDigit(name.charAt(idx))) {
          idx -= 1
        }
        while (idx >= 0 && name.charAt(idx) == '$') {
          idx -= 1
        }
      }

      // Include until the next '$' (inner class) or '.' (top-level class)
      while (idx >= 0 && {
        val currChar = name.charAt(idx)
        currChar != '.' && currChar != '$'
      }) {
        idx -= 1
      }

      name.substring(idx + 1)
    }
  }

  def getSuperclass(): Class[_ >: A] =
    data.getSuperclass()

  def getComponentType(): Class[_] =
    data.getComponentType()

  @inline
  def cast(obj: Any): A = {
    getData().checkCast(obj)
    obj.asInstanceOf[A]
  }

  // java.lang.reflect.Array support

  private[lang] def newArrayOfThisClass(dimensions: js.Array[Int]): AnyRef =
    data.newArrayOfThisClass(dimensions)
}
