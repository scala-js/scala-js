package org.scalajs.linker.standard

import org.scalajs.linker._

final class LinkingUnit private[linker] (
    val coreSpec: CoreSpec,
    val classDefs: List[LinkedClass],
    val moduleInitializers: List[ModuleInitializer]
)
