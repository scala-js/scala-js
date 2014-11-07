package scala.scalajs.tools.classpath

import scala.scalajs.tools.jsdep.Origin

/** Expresses a requirement for a given semantic to be compliant */
final class ComplianceRequirement(
    val semantics: String, val origins: List[Origin])
