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

import org.scalajs.linker._

final class LinkingUnit private[linker] (
    val coreSpec: CoreSpec,
    val classDefs: List[LinkedClass],
    val moduleInitializers: List[ModuleInitializer]
)
