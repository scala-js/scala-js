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

import java.nio.ByteBuffer
import java.util.Base64

import org.scalajs.ir

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.standard.MemClassDefIRFileImpl

object PrivateLibHolder {
  private val stableVersion = ir.Version.fromInt(0) // never changes

  val files: Seq[IRFile] = {
    for ((name, contentBase64) <- PrivateLibData.pathsAndContents) yield {
      val path = "org/scalajs/linker/runtime/" + name
      val content = Base64.getDecoder().decode(contentBase64)
      val tree = ir.Serializers.deserialize(ByteBuffer.wrap(content))
      val patchedTree = PrivateLibPatches.patchClassDef(tree)
      new MemClassDefIRFileImpl(path, stableVersion, patchedTree)
    }
  }
}
