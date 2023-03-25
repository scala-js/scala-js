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

package org.scalajs.linker.testutils

import scala.concurrent._

import org.scalajs.ir.Trees.ClassDef
import org.scalajs.ir.Version

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.standard.MemClassDefIRFileImpl

object MemClassDefIRFile {
  def apply(classDef: ClassDef): IRFile =
    apply(classDef, Version.Unversioned)

  def apply(classDef: ClassDef, version: Version): IRFile = {
    val path = "mem://" + classDef.name.name.nameString + ".sjsir"
    new MemClassDefIRFileImpl(path, version, classDef)
  }
}
