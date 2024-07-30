/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.standard

import org.scalajs.linker.interface._

/** Core specification for the Scala.js code. */
final class CoreSpec private (
    /** Scala.js semantics. */
    val semantics: Semantics,
    /** Module kind. */
    val moduleKind: ModuleKind,
    /** ECMAScript features to use. */
    val esFeatures: ESFeatures
) {
  import CoreSpec._

  /** Link-time resolved properties */
  val linkTimeProperties = new LinkTimeProperties(
    semantics, esFeatures)

  override def equals(that: Any): Boolean = that match {
    case that: CoreSpec =>
      this.semantics == that.semantics &&
      this.moduleKind == that.moduleKind &&
      this.esFeatures == that.esFeatures
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, semantics.##)
    acc = mix(acc, moduleKind.##)
    acc = mixLast(acc, esFeatures.##)
    finalizeHash(acc, 3)
  }

  override def toString(): String = {
    s"""CoreSpec(
       |  semantics  = $semantics,
       |  moduleKind = $moduleKind,
       |  esFeatures = $esFeatures
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
        esFeatures = ESFeatures.Defaults)
  }

  private[linker] def apply(
      semantics: Semantics,
      moduleKind: ModuleKind,
      esFeatures: ESFeatures): CoreSpec = {
    new CoreSpec(semantics, moduleKind, esFeatures)
  }
}
