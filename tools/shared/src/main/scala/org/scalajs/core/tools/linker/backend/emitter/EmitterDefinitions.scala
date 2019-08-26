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

package org.scalajs.core.tools.linker.backend.emitter

import org.scalajs.core.ir.Definitions._

private[emitter] object EmitterDefinitions {
  /* In theory, some of the following could be computed from the Class
   * Hierarchy. However, that would be require dealing with incremental runs,
   * which would be overkill since these things are in fact known to be static.
   */

  final val NumberClass = "jl_Number"

  val HijackedClassesAndTheirSuperClasses =
    HijackedClasses ++ Set(ObjectClass, NumberClass)

}
