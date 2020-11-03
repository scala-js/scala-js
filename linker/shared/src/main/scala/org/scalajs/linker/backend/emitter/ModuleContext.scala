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

package org.scalajs.linker.backend.emitter

import org.scalajs.linker.standard.ModuleSet
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Provides information about the module a piece of code is generated in. */
private[emitter] final class ModuleContext private (
    val moduleID: ModuleID, val public: Boolean
) {
  override def equals(that: Any): Boolean = that match {
    case that: ModuleContext =>
      this.moduleID == that.moduleID &&
        this.public == that.public
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = ModuleContext.HashSeed
    acc = mix(acc, moduleID.##)
    acc = mixLast(acc, public.##)
    finalizeHash(acc, 2)
  }
}

object ModuleContext {
  def fromModule(module: ModuleSet.Module): ModuleContext =
    new ModuleContext(module.id, module.public)

  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[ModuleContext].getName)
}
