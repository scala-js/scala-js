/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

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
