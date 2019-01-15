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

/* This is a hijacked class. Its only instance is the value 'undefined'.
 * Constructors are not emitted.
 *
 * On the JVM, this class has no instance. In Scala.js, it is repurposed as the
 * boxed class for unit, aka `void`. The instance methods are
 * Scala.js-specific.
 */
final class Void private () extends AnyRef {
  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int = 0

  @inline override def toString(): String = "undefined"
}

object Void {
  final val TYPE = classOf[scala.Unit]
}
