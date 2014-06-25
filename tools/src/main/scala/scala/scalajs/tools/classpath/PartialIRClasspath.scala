/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath

import scala.collection.immutable.{Seq, Traversable}

import scala.scalajs.tools.io._
import scala.scalajs.tools.logging._
import scala.scalajs.tools.jsdep._
import scala.scalajs.tools.packager._
import scala.scalajs.tools.sourcemap._

import java.io.File

/** A PartialClasspath whose scalaJSCode consists only of IR */
class PartialIRClasspath(
    dependencies: Traversable[JSDependencyManifest],
    availableLibs: Map[String, VirtualJSFile],
    /** Scala.js IR contained in this PartialClasspath.
     *  Its ordering is implicit (using ancestor count) */
    val scalaJSIR: Traversable[VirtualScalaJSIRFile],
    version: Option[String]
) extends PartialClasspath(dependencies, availableLibs, version) {

  import PartialClasspath.DependencyFilter

  /** Orders and desugars the contained IR.
   *
   *  Consider using ScalaJSPackager for a canonical way to do so. It allows to
   *  persist the resulting file and create a source map.
   */
  override def scalaJSCode: Seq[VirtualJSFile] = {
    import ScalaJSPackager._

    val output = WritableMemVirtualJSFile("temporary-package.js")
    (new ScalaJSPackager).packageIR(scalaJSIR, OutputConfig(output),
        NullLogger)

    output :: Nil
  }

  override def resolve(filter: DependencyFilter): CompleteIRClasspath =
    new CompleteIRClasspath(resolveDependencies(filter), scalaJSIR, version)

  /** The same as append, but does not preserve order between the IR files
   *  of both PartialIRClasspaths. This is always safe (if append is safe),
   *  since we can establish a safe order using ancestor count.
   *
   *  It is **strongly** advisable to use merge over append whenever possible,
   *  since append needs to read the scalaJSCode property which triggers a
   *  packaging step.
   */
  def merge(that: PartialIRClasspath): PartialIRClasspath = {
    new PartialIRClasspath(
        this.dependencies  ++ that.dependencies,
        this.availableLibs ++ that.availableLibs,
        this.scalaJSIR     ++ that.scalaJSIR,
        CacheUtils.joinVersions(this.version, that.version))
  }
}
