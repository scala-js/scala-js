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

package org.scalajs.ir

import Names._

/** An optional original name.
 *
 *  Since an `OriginalName` is basically an optional `UTF8String`, original
 *  names must always be well-formed Unicode strings. Unpaired surrogates are
 *  not valid.
 */
final class OriginalName private (private val bytes: Array[Byte])
    extends AnyVal {

  /* The underlying field is a `bytes` instead of a `UTF8String` for two
   * reasons:
   * - a `UTF8String` cannot be `null`, and
   * - the underlying val of a value class cannot itself be a custom value
   *   class.
   */

  def isEmpty: Boolean = bytes == null
  def isDefined: Boolean = bytes != null

  /** Gets the underlying `UTF8String` without checking for emptiness. */
  @inline private def unsafeGet: UTF8String =
    UTF8String.unsafeCreate(bytes)

  def get: UTF8String = {
    if (isEmpty)
      throw new NoSuchElementException("NoOriginalName.get")
    unsafeGet
  }

  def orElse(name: Name): OriginalName =
    orElse(name.encoded)

  def orElse(name: MethodName): OriginalName =
    orElse(name.simpleName)

  def orElse(name: UTF8String): OriginalName =
    if (isDefined) this
    else OriginalName(name)

  def getOrElse(name: Name): UTF8String =
    getOrElse(name.encoded)

  def getOrElse(name: MethodName): UTF8String =
    getOrElse(name.simpleName)

  def getOrElse(name: UTF8String): UTF8String =
    if (isDefined) unsafeGet
    else name

  def getOrElse(name: String): UTF8String = {
    /* Do not use `getOrElse(UTF8Sring(name))` so that we do not pay the cost
     * of encoding the `name` in UTF-8 if `this.isDefined`.
     */
    if (isDefined) unsafeGet
    else UTF8String(name)
  }

  override def toString(): String =
    if (isDefined) s"OriginalName($unsafeGet)"
    else "NoOriginalName"
}

object OriginalName {
  val NoOriginalName: OriginalName = new OriginalName(null)

  def apply(name: UTF8String): OriginalName =
    new OriginalName(name.bytes)

  def apply(name: Name): OriginalName =
    apply(name.encoded)

  def apply(name: MethodName): OriginalName =
    apply(name.simpleName)

  def apply(name: String): OriginalName =
    apply(UTF8String(name))
}
