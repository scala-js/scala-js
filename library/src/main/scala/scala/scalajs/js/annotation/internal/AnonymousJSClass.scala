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

/** IMPLEMENTATION DETAIL: Marks anonymous non-native JS classes.
 *
 *  This annotation is added automatically by the compiler to anonymous
 *  JS classes (that are not lambdas).
 *
 *  Do not use this annotation yourself.
 */
class AnonymousJSClass extends scala.annotation.Annotation
