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

/** Stripped down version of `java.lang.Throwable` with the bare minimum to
 *  support `toString()` and the subclasses.
 *
 *  We cannot use the full `java.lang.Throwable` out of the box, because its
 *  constructor calls `initStackTrace()`, which uses `StackTrace.scala` and
 *  therefore a bunch of JS stuff inside to recover stack traces. This stripped
 *  down `Throwable` does not offer any method to access the stack trace so
 *  that `initStackTrace()` is not necessary.
 */
class Throwable(s: String, e: Throwable)
    extends Object with java.io.Serializable {

  def this() = this(null, null)
  def this(s: String) = this(s, null)
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)

  def getMessage(): String = s
  def getCause(): Throwable = e

  override def toString(): String = {
    val className = getClass.getName
    val message = getMessage()
    if (message eq null) className
    else className + ": " + message
  }
}
