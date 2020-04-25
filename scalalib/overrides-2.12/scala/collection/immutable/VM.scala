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

package scala.collection.immutable

// Backport from scala.runtime moved into s.c.immutable and made package
// private to avoid the need for MiMa whitelisting.
/* private[immutable] */ object VM {
  def releaseFence(): Unit = ()
}
