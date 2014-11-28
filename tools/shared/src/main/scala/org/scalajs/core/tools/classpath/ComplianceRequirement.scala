package org.scalajs.core.tools.classpath

import org.scalajs.core.tools.jsdep.Origin

/** Expresses a requirement for a given semantic to be compliant */
final class ComplianceRequirement(
    val semantics: String, val origins: List[Origin])
