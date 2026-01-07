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

package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** <span class="badge badge-ecma2021" style="float: right;">ECMAScript 2021</span>
 *
 *  A WeakRef object lets you hold a weak reference to another object, without preventing
 *  that object from getting garbage-collected.
 *
 *  MDN
 *
 *  @param targetObject An object whose weak reference contained by an instance of WeakRef.
 *  @tparam T unconstrained, but the constructor will throw a TypeError if targetObject is not an Object.
 */
@js.native
@JSGlobal
class WeakRef[+T](targetObject: T) extends js.Object {

  /** The target object of the WeakRef, or undefined if the object has been garbage-collected. */
  def deref[S >: T](): js.UndefOr[S] = js.native
}
