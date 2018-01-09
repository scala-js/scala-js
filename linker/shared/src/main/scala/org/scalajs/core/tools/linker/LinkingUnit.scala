package org.scalajs.core.tools.linker

import org.scalajs.core.tools.linker.standard._

final class LinkingUnit private[linker] (
    val coreSpec: CoreSpec,
    val classDefs: List[LinkedClass],
    val moduleInitializers: List[ModuleInitializer]
)
