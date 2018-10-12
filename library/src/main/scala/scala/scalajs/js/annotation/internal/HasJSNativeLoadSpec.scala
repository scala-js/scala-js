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

package scala.scalajs.js.annotation.internal

/** IMPLEMENTATION DETAIL: Marks this JS native module class as having stored
 *  a loading spec in its IR.
 *
 *  This is true iff the module class was compiled with Scala.js 0.6.13 or
 *  later.
 *
 *  Do not use this annotation yourself.
 */
class HasJSNativeLoadSpec extends scala.annotation.StaticAnnotation
