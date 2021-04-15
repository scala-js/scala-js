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
 *  A FinalizationRegistry object lets you request a callback when an object is garbage-collected.
 *
 *  FinalizationRegistry provides a way to request that a cleanup callback get called at some point
 *  when an object registered with the registry has been reclaimed (garbage-collected).
 *  (Cleanup callbacks are sometimes called finalizers.)
 *
 *  MDN
 *
 *  @tparam A Type of the target object to register
 *  @tparam B Type of value to pass to the finalizer
 *  @tparam C Type of an unregistration token
 */
@js.native
@JSGlobal
class FinalizationRegistry[-A, -B, -C](finalizer: js.Function1[B, scala.Any]) extends js.Object {
  /** The `register` method registers an object with a FinalizationRegistry instance so that if
   *  the object is garbage-collected, the registry's callback may get called.
   *
   *  @param theObject The target object to register.
   *  @param heldValue The value to pass to the finalizer for this object. This cannot be the target object.
   *  @param unregistrationToken
   *    A token that may be used with the `unregister` method later to
   *    unregister the target object. If provided (and not `undefined`),
   *    this must be an object. If not provided, the target cannot be unregistered.
   */
  def register(theObject: A, heldValue: B, unregistrationToken: C): Unit = js.native

  /** The `register` method registers an object with a FinalizationRegistry instance so that if
   *  the object is garbage-collected, the registry's callback may get called.
   *
   *  @param theObject The target object to register.
   *  @param heldValue The value to pass to the finalizer for this object. This cannot be the target object.
   */
  def register(theObject: A, heldValue: B): Unit = js.native

  /** The `unregister`` unregisters a target object from a `FinalizationRegistry` instance.
   *
   *  When a target object has been reclaimed, it is no longer registered in the registry.
   *  There is no need to all `unregister` in your cleanup callback. Only call `unregister` if
   *  you haven't received a cleanup callback and no longer need to receive one.
   *
   *  @param unregistrationToken The token used with the register method when registering the target object.
   */
  def unregister(unregistrationToken: C): Unit = js.native
}
