/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.standard

import org.scalajs.linker._

/** Core specification for the Scala.js code. */
final class CoreSpec private (
    /** Scala.js semantics. */
    val semantics: Semantics,
    /** Module kind. */
    val moduleKind: ModuleKind,
    /** Standard output mode. */
    val outputMode: OutputMode
) {
  import CoreSpec._

  override def equals(that: Any): Boolean = that match {
    case that: CoreSpec =>
      this.semantics == that.semantics &&
      this.moduleKind == that.moduleKind &&
      this.outputMode == that.outputMode
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, semantics.##)
    acc = mix(acc, moduleKind.##)
    acc = mixLast(acc, outputMode.##)
    finalizeHash(acc, 3)
  }

  override def toString(): String = {
    s"""CoreSpec(
       |  semantics  = $semantics,
       |  moduleKind = $moduleKind,
       |  outputMode = $outputMode
       |)""".stripMargin
  }
}

private[linker] object CoreSpec {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[CoreSpec].getName)

  private[linker] val Defaults: CoreSpec = {
    new CoreSpec(
        semantics = Semantics.Defaults,
        moduleKind = ModuleKind.NoModule,
        outputMode = OutputMode.Default)
  }

  private[linker] def apply(
      semantics: Semantics,
      moduleKind: ModuleKind,
      outputMode: OutputMode): CoreSpec = {
    new CoreSpec(semantics, moduleKind, outputMode)
  }
}
