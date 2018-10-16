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

import scala.scalajs.js

/** Stripped down version of `java.lang.Class`, with just the bare minimum to
 *  support `toString()`.
 *
 *  We cannot use the full `java.lang.Class` out of the box because it
 *  internally uses a static facade type for its `data`, and the minilib cannot
 *  contain any JS type.
 */
final class Class[A] private (private val data: Object) extends Object {
  override def toString(): String = {
    (if (isInterface()) "interface " else
        if (isPrimitive()) "" else "class ") + getName()
  }

  def isInterface(): scala.Boolean =
    data.asInstanceOf[js.Dynamic].isInterface.asInstanceOf[scala.Boolean]

  def isPrimitive(): scala.Boolean =
    data.asInstanceOf[js.Dynamic].isPrimitive.asInstanceOf[scala.Boolean]

  def getName(): String =
    data.asInstanceOf[js.Dynamic].name.asInstanceOf[String]
}
