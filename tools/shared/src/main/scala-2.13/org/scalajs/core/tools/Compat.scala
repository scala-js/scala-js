package org.scalajs.core.tools

import scala.collection.Iterable

private[tools] object Compat {

  type NonDeprecatedTraversable[+A] = Iterable[A]
  type NonDeprecatedImmutableTraversable[+A] = scala.collection.immutable.Iterable[A]

}