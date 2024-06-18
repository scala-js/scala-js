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
    val esFeatures: ESFeatures,
    /** Whether we are compiling to WebAssembly. */
    val targetIsWebAssembly: Boolean
) {
  import CoreSpec._

  private def this() = {
    this(
      semantics = Semantics.Defaults,
      moduleKind = ModuleKind.NoModule,
      esFeatures = ESFeatures.Defaults,
      targetIsWebAssembly = false
    )
  }

  def withSemantics(semantics: Semantics): CoreSpec =
    copy(semantics = semantics)

  def withSemantics(f: Semantics => Semantics): CoreSpec =
    copy(semantics = f(semantics))

  def withModuleKind(moduleKind: ModuleKind): CoreSpec =
    copy(moduleKind = moduleKind)

  def withESFeatures(esFeatures: ESFeatures): CoreSpec =
    copy(esFeatures = esFeatures)

  def withESFeatures(f: ESFeatures => ESFeatures): CoreSpec =
    copy(esFeatures = f(esFeatures))

  def withTargetIsWebAssembly(targetIsWebAssembly: Boolean): CoreSpec =
    copy(targetIsWebAssembly = targetIsWebAssembly)
  /** Link-time resolved properties */

  val linkTimeProperties = new LinkTimeProperties(
    semantics, esFeatures)

  override def equals(that: Any): Boolean = that match {
    case that: CoreSpec =>
      this.semantics == that.semantics &&
      this.moduleKind == that.moduleKind &&
      this.esFeatures == that.esFeatures &&
      this.targetIsWebAssembly == that.targetIsWebAssembly
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, semantics.##)
    acc = mix(acc, moduleKind.##)
    acc = mix(acc, esFeatures.##)
    acc = mixLast(acc, targetIsWebAssembly.##)
    finalizeHash(acc, 4)
  }

  override def toString(): String = {
    s"""CoreSpec(
       |  semantics  = $semantics,
       |  moduleKind = $moduleKind,
       |  esFeatures = $esFeatures,
       |  targetIsWebAssembly = $targetIsWebAssembly
       |)""".stripMargin
  }

  private def copy(
      semantics: Semantics = semantics,
      moduleKind: ModuleKind = moduleKind,
      esFeatures: ESFeatures = esFeatures,
      targetIsWebAssembly: Boolean = targetIsWebAssembly
  ): CoreSpec = {
    new CoreSpec(
      semantics,
      moduleKind,
      esFeatures,
      targetIsWebAssembly
    )
  }
}

private[linker] object CoreSpec {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[CoreSpec].getName)

  val Defaults: CoreSpec = new CoreSpec()

  private[linker] def fromStandardConfig(config: StandardConfig): CoreSpec = {
    new CoreSpec(
      config.semantics,
      config.moduleKind,
      config.esFeatures,
      config.experimentalUseWebAssembly
    )
  }
}
