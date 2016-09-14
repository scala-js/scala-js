package org.scalajs.core.tools.jsdep

import org.scalajs.core.tools.sem.Semantics

/** Expresses a requirement for a given semantic to be compliant */
final class ComplianceRequirement(
    val semantics: String, val origins: List[Origin]) {
  override def toString(): String =
    s"ComplianceRequirement($semantics, origins = [${origins.mkString(", ")}])"

  override def equals(that: Any): Boolean = that match {
    case that: ComplianceRequirement =>
      this.semantics == that.semantics &&
      this.origins == that.origins
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = ComplianceRequirement.HashSeed
    acc = mix(acc, semantics.##)
    acc = mixLast(acc, origins.##)
    finalizeHash(acc, 2)
  }
}

object ComplianceRequirement {
  // "org.scalajs.core.tools.jsdep.ComplianceRequirement".##
  private final val HashSeed = -1738348249

  /** Checks whether the given semantics are compliant with the given
   *  requirements.
   *  @throws BadComplianceException if the semantics are not compliant.
   */
  final def checkCompliance(requirements: Traversable[ComplianceRequirement],
      semantics: Semantics): Unit = {
    val unmet = requirements.filterNot(compliance =>
        semantics.isCompliant(compliance.semantics))

    if (unmet.nonEmpty)
      throw new BadComplianceException(unmet.toList)
  }

  def mergeFromManifests(
      manifests: Traversable[JSDependencyManifest]
  ): Traversable[ComplianceRequirement] = {

    val flatTups = for {
      manifest <- manifests
      semantics <- manifest.compliantSemantics
    } yield (semantics, manifest.origin)

    for {
      (semantics, tups) <- flatTups.groupBy(_._1)
    } yield new ComplianceRequirement(semantics, tups.map(_._2).toList)
  }

}
