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

package org.scalajs.linker.standard

/** A versioned thing, accompanied by its version.
 *
 *  Note that, as used in `LinkingUnit`, the [[version]] is relative to the
 *  identity of the versioned thing. The definition of identity varies as
 *  items progress through the linking pipeline, but it only gets stronger,
 *  i.e., if two items are id-different at phase P, then they must also be
 *  id-different at phase P+1. The converse is not true. This guarantees that
 *  versions can be reliably used to determine at phase P+1 whether the given
 *  item coming from phase P must be reprocessed.
 */
final class Versioned[+T](val value: T, val version: Option[String])
