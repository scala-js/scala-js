/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

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
