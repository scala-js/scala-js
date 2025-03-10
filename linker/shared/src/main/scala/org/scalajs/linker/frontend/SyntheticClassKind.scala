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

package org.scalajs.linker.frontend

import org.scalajs.ir.Trees.NewLambda

sealed abstract class SyntheticClassKind

object SyntheticClassKind {
  final case class Lambda(descriptor: NewLambda.Descriptor) extends SyntheticClassKind
}
