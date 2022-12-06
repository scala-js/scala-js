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

package scala.scalajs.js.annotation

/** Mark a concrete trait method as a Java default method.
 *
 *  This annotation can be used on concrete trait methods to mark them as
 *  Java default methods. This should be used *only* to implement interfaces
 *  of the JDK that have default methods in Java.
 *
 *  Otherwise using this annotation is unspecified.
 */
@deprecated("Has no effect in Scala 2.12+ (default methods are the default). Remove", "1.13.0")
class JavaDefaultMethod extends scala.annotation.StaticAnnotation
