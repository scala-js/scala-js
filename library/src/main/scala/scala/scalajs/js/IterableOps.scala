/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.scalajs.js

/** Adapts a JavaScript Iterable to a Scala Iterable */
@inline
final class IterableOps[+A](self: js.Iterable[A])
    extends scala.collection.Iterable[A] {
  @inline
  def iterator: scala.collection.Iterator[A] = self.jsIterator().toIterator
}
