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

/** IMPLEMENTATION DETAIL: Marks the annotated member as exposed as a JS member.
 *
 *  This annotation is added automatically by the compiler to all public and
 *  protected members of a non-native JS class. It marks the annotated
 *  member as being exposed as a JS member.
 *
 *  Do not use this annotation yourself.
 */
class ExposedJSMember extends scala.annotation.StaticAnnotation
