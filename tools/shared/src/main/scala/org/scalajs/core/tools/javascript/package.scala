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

package object javascript {
  // Backward source compatibility for build files

  @deprecated("Use org.scalajs.core.tools.linker.backend.OutputMode instead.", "0.6.6")
  type OutputMode = org.scalajs.core.tools.linker.backend.OutputMode

  @deprecated("Use org.scalajs.core.tools.linker.backend.OutputMode instead.", "0.6.6")
  lazy val OutputMode = org.scalajs.core.tools.linker.backend.OutputMode

}
