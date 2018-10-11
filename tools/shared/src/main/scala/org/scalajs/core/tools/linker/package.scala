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

package org.scalajs.core.tools

package object linker {
  type Semantics = org.scalajs.core.tools.sem.Semantics

  val Semantics: org.scalajs.core.tools.sem.Semantics.type =
    org.scalajs.core.tools.sem.Semantics

  type CheckedBehavior = org.scalajs.core.tools.sem.CheckedBehavior

  val CheckedBehavior: org.scalajs.core.tools.sem.CheckedBehavior.type =
    org.scalajs.core.tools.sem.CheckedBehavior

  type ModuleKind = org.scalajs.core.tools.linker.backend.ModuleKind

  val ModuleKind: org.scalajs.core.tools.linker.backend.ModuleKind.type =
    org.scalajs.core.tools.linker.backend.ModuleKind

  type ESFeatures = org.scalajs.core.tools.linker.backend.OutputMode

  val ESFeatures: org.scalajs.core.tools.linker.backend.OutputMode.type =
    org.scalajs.core.tools.linker.backend.OutputMode
}
