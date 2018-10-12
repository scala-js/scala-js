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

package scala.runtime

/* This is a hijacked class. Its only instance is the value 'undefined'.
 * Constructors are not emitted.
 */
class BoxedUnit private () extends AnyRef with java.io.Serializable {
  @inline override def equals(that: Any): Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int = 0

  @inline override def toString(): String = "undefined"
}

object BoxedUnit {
  def UNIT: BoxedUnit = throw new Error("stub")

  final val TYPE = classOf[Unit]
}
