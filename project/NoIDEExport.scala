package build

import sbt._
import Keys._

/** Settings to prevent projects from being exported to IDEs. */
object NoIDEExport {
  /* We detect whether bloop is on the classpath (which will be the case during
   * import in Metals) and if yes, we deactivate the bloop export for
   * irrelevant projects.
   *
   * Notably, we deactivate all non-2.12 projects in `MultiScalaProject`, since
   * it is enough to have IDE support for the default Scala version.
   */

  private lazy val bloopGenerateKey: Option[TaskKey[Option[File]]] = {
    val optBloopKeysClass = try {
      Some(Class.forName("bloop.integrations.sbt.BloopKeys"))
    } catch {
      case _: ClassNotFoundException => None
    }

    optBloopKeysClass.map { bloopKeysClass =>
      val bloopGenerateGetter = bloopKeysClass.getMethod("bloopGenerate")
      bloopGenerateGetter.invoke(null).asInstanceOf[TaskKey[Option[File]]]
    }
  }

  /** Settings to prevent the project from being exported to IDEs. */
  lazy val noIDEExportSettings: Seq[Setting[_]] = {
    bloopGenerateKey match {
      case None =>
        Nil
      case Some(key) =>
        Seq(
            key in Compile := None,
            key in Test := None,
        )
    }
  }
}
