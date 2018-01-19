package org.scalajs.linker

import org.scalajs.linker.standard._

final class LinkingUnit private[linker] (
    val coreSpec: CoreSpec,
    val classDefs: List[LinkedClass],
    val moduleInitializers: List[ModuleInitializer]
)
