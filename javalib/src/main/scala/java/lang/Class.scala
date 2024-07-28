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

final class Class[A] private ()
    extends Object with Serializable with Constable {

  private[this] var cachedSimpleName: String = _

  override def toString(): String = {
    (if (isInterface()) "interface " else
        if (isPrimitive()) "" else "class ")+getName()
  }

  @inline
  def isInstance(obj: Any): scala.Boolean =
    throw new Error("Stub filled in by the compiler")

  @inline
  def isAssignableFrom(that: Class[_]): scala.Boolean =
    throw new Error("Stub filled in by the compiler")

  @inline
  def isInterface(): scala.Boolean =
    throw new Error("Stub filled in by the compiler")

  @inline
  def isArray(): scala.Boolean =
    throw new Error("Stub filled in by the compiler")

  @inline
  def isPrimitive(): scala.Boolean =
    throw new Error("Stub filled in by the compiler")

  @inline
  def getName(): String =
    throw new Error("Stub filled in by the compiler")

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
      val name = getName()
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

  @inline
  def getSuperclass(): Class[_ >: A] =
    throw new Error("Stub filled in by the compiler")

  @inline
  def getComponentType(): Class[_] =
    throw new Error("Stub filled in by the compiler")

  @inline
  def cast(obj: Any): A =
    throw new Error("Stub filled in by the compiler")

  // java.lang.reflect.Array support

  @inline
  private[lang] def newArrayOfThisClass(length: Int): AnyRef =
    throw new Error("Stub filled in by the compiler")
}
