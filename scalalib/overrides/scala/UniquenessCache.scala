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

package scala

import scala.scalajs.js

// Used in the various Symbol.scala files spread in version-dependent overrides.
private[scala] abstract class UniquenessCache[V >: Null] {
  private val safeHasOwnProperty = {
    js.Dynamic.global.Object.prototype.hasOwnProperty
      .asInstanceOf[js.ThisFunction1[js.Dynamic, String, Boolean]]
  }

  private val cache = new js.Object().asInstanceOf[js.Dynamic]

  protected def valueFromKey(k: String): V
  protected def keyFromValue(v: V): Option[String]

  def apply(name: String): V = {
    val cache = this.cache // local copy
    if (safeHasOwnProperty(cache, name)) {
      cache.selectDynamic(name).asInstanceOf[V]
    } else {
      val value = valueFromKey(name)
      cache.updateDynamic(name)(value.asInstanceOf[js.Any])
      value
    }
  }

  def unapply(other: V): Option[String] = keyFromValue(other)
}
