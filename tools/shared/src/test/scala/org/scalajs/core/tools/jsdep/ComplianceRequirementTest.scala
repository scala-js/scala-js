package org.scalajs.core.tools.jsdep

import org.scalajs.core.tools.sem.Semantics

import org.junit.Test
import org.junit.Assert._

class ComplianceRequirementTest {
  private val origin = new Origin("test", "compile")
  private val semantics = Set("asInstanceOfs", "moduleInit", "strictFloats")

  private def mkComplianceRequirement(semantics: String) =
    new ComplianceRequirement(semantics, origin :: Nil)

  @Test
  def checkComplianceSuccess: Unit = {
    for (sems <- semantics.subsets) {
      val origin = new Origin("test", "compile")
      val requirements = sems.map(mkComplianceRequirement)
      val semantics = Semantics.compliantTo(sems)
      ComplianceRequirement.checkCompliance(requirements, semantics)
    }
  }

  @Test
  def checkComplianceFail: Unit = {
    for {
      required <- semantics.subsets
      missing <- required.subsets
      if missing.nonEmpty
    } {
      val present = required -- missing
      val requirements = required.map(mkComplianceRequirement)
      val semantics = Semantics.compliantTo(present)
      try {
        ComplianceRequirement.checkCompliance(requirements, semantics)
        fail("Expected a BadComplianceException be thrown")
      } catch {
        case e: BadComplianceException =>
          val expected = requirements.filterNot(r => present(r.semantics))
          assertEquals(expected, e.unmet.toSet)
      }
    }
  }

  @Test
  def mergeFromManifests: Unit = {
    val origins = Array("a", "b", "c").map(new Origin(_, "compile"))

    def mkManifest(o: Int, semantics: String*) =
      new JSDependencyManifest(origins(o), Nil, false, semantics.toList)

    val manifests = Seq(
        mkManifest(0, "isInstanceOfs"),
        mkManifest(1, "isInstanceOfs", "moduleInit"),
        mkManifest(2))

    val result = ComplianceRequirement.mergeFromManifests(manifests).toSet
    val expected = Set(
        new ComplianceRequirement("isInstanceOfs", List(0, 1).map(origins)),
        new ComplianceRequirement("moduleInit", List(1).map(origins)))

    assertEquals(expected, result)
  }

}
