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

/* Even though this trait belongs to the java.lang package, we compile it as
 * part of javalib instead of javalanglib, so that classes and interfaces in
 * the javalib can inherit from AutoCloseable, even when we compile with JDK 6.
 * It turns out that no class in java.lang needs to inherit from this trait,
 * fortunately.
 */
trait AutoCloseable {
  def close(): Unit
}
