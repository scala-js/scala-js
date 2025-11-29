package build

import sbt._
import Keys._
import Def.SettingsDefinition

final class MultiScalaProject private (private val projects: Map[String, Project])
    extends CompositeProject {
  import MultiScalaProject._

  val v2_12: Project = projects("2.12")
  def v2_13: Project = projects("2.13")
  def v3: Project = projects("3")

  def settings(ss: SettingsDefinition*): MultiScalaProject =
    transform(_.settings(ss: _*))

  def enablePlugins(ns: Plugins*): MultiScalaProject =
    transform(_.enablePlugins(ns: _*))

  def dependsOn(deps: ScopedMultiScalaProject*): MultiScalaProject = {
    def classpathDependency(d: ScopedMultiScalaProject) =
      strictMapValues(d.project.projects)(ClasspathDependency(_, d.configuration))

    val depsByVersion: Map[String, Seq[ClasspathDependency]] =
      strictMapValues(deps.flatMap(classpathDependency).groupBy(_._1))(_.map(_._2))
    zipped(depsByVersion)(_.dependsOn(_: _*))
  }

  def dependsOn(deps: ClasspathDependency*)(implicit dummy: DummyImplicit): MultiScalaProject =
    transform(_.dependsOn(deps: _*))

  def configs(cs: Configuration*): MultiScalaProject =
    transform(_.configs(cs: _*))

  def zippedSettings(that: MultiScalaProject)(ss: Project => SettingsDefinition): MultiScalaProject =
    zipped(that.projects)((p, sp) => p.settings(ss(sp)))

  def zippedSettings(project: String)(ss: LocalProject => SettingsDefinition): MultiScalaProject =
    zippedSettings(Seq(project))(ps => ss(ps(0)))

  /** Set settings on this MultiScalaProject depending on other MultiScalaProjects by name.
   *
   *  For every Scala version of this MultiScalaProject, `ss` is invoked onced
   *  with a LocalProjects corresponding to the names in projectNames with a
   *  suffix for that version.
   */
  def zippedSettings(projectNames: Seq[String])(
      ss: Seq[LocalProject] => SettingsDefinition): MultiScalaProject = {
    val ps = for {
      (v, p) <- projects
    } yield {
      val lps = projectNames.map(pn => LocalProject(projectID(pn, v)))
      v -> p.settings(ss(lps))
    }
    new MultiScalaProject(ps)
  }

  def %(configuration: String) = new ScopedMultiScalaProject(this, Some(configuration))

  override def componentProjects: Seq[Project] = projects.valuesIterator.toSeq

  private def zipped[T](that: Map[String, T])(f: (Project, T) => Project): MultiScalaProject = {
    val ps = for ((v, p) <- projects) yield v -> f(p, that(v))
    new MultiScalaProject(ps)
  }

  private def transform(f: Project => Project): MultiScalaProject =
    new MultiScalaProject(strictMapValues(projects)(f))
}

final class ScopedMultiScalaProject(val project: MultiScalaProject, val configuration: Option[String])

object ScopedMultiScalaProject {
  implicit def fromMultiScalaProject(mp: MultiScalaProject): ScopedMultiScalaProject =
    new ScopedMultiScalaProject(mp, None)
}

object MultiScalaProject {
  private def strictMapValues[K, U, V](v: Map[K, U])(f: U => V): Map[K, V] =
    v.map(v => (v._1, f(v._2)))

  private final val ideVersion = "2.12"

  private def projectID(id: String, major: String) = id + major.replace('.', '_')

  def apply(id: String, base: File, scalaVersions: Seq[String] = Seq("2.12", "2.13")): MultiScalaProject = {
    import ExposedValues.autoImport._

    val projects = for {
      major <- scalaVersions
    } yield {
      val noIDEExportSettings =
        if (major == ideVersion) Nil
        else NoIDEExport.noIDEExportSettings

      val (crossVersionsKey, defaultVersionKey) = major match {
        case "2.12" => (cross212ScalaVersions, default212ScalaVersion)
        case "2.13" => (cross213ScalaVersions, default213ScalaVersion)
        case "3" => (cross3ScalaVersions, default3ScalaVersion)
      }

      major -> Project(id = projectID(id, major), base = new File(base, "." + major)).settings(
        crossScalaVersions := crossVersionsKey.value,
        scalaVersion := defaultVersionKey.value,
        noIDEExportSettings,
      )
    }

    new MultiScalaProject(projects.toMap).settings(
      sourceDirectory := baseDirectory.value.getParentFile / "src",
    )
  }
}
