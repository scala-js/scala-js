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

import org.scalajs.linker.standard.ModuleSet.ModuleID

private[emitter] final class ModuleContext(
    val moduleID: ModuleID,
    val public: Boolean
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
    acc = mix(acc, public.##)
    finalizeHash(acc, 2)
  }
}

private object ModuleContext {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[ModuleContext].getName)
}
