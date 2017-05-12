package org.scalajs.jsdependencies.core

import org.junit.Test
import org.junit.Assert._

class ManifestFiltersTest {
  import ManifestFilters._

  private def mkManifest(module: String, deps: String*) = {
    new JSDependencyManifest(new Origin(module, "compile"),
        deps.map(new JSDependency(_)).toList, requiresDOM = false)
  }

  @Test
  def reinterpretResourceNamesSimple(): Unit = {
    val filter = reinterpretResourceNames("lib-b")(
        "jquery.js" -> "2.1.3/jquery.js")

    val otherManifest = mkManifest("lib-a", "2.1.4/jquery.js", "bar.js")
    val targetManifest = mkManifest("lib-b", "jquery.js", "foo.js")

    val result = filter(Seq(otherManifest, targetManifest)).toSet
    val expected = Set(otherManifest,
        mkManifest("lib-b", "2.1.3/jquery.js", "foo.js"))

    assertEquals(expected, result)
  }

  @Test
  def reinterpretResourceNamesFull(): Unit = {
    val filter = reinterpretResourceNames(
        origin => oldName => origin.moduleName + oldName)

    val manifests = for {
      origin <- Seq("oa", "ob", "oc")
    } yield {
      val oldLibs = Seq("a.js", "b.js", "c.js")
      val newLibs = oldLibs.map(origin + _)
      (mkManifest(origin, oldLibs: _*), mkManifest(origin, newLibs: _*))
    }

    val (input, expected) = manifests.unzip
    val result = filter(input)
    assertEquals(expected, result)
  }
}
