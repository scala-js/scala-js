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

import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.Trees.ClassDef

import org.scalajs.linker.IRFile
import org.scalajs.linker.standard.IRFileImpl

private final class MemClassDefIRFile(classDef: ClassDef)
    extends IRFileImpl("mem://" + classDef.name.name + ".sjsir", None) {

  def tree(implicit ec: ExecutionContext): Future[ClassDef] =
    Future(classDef)

  def entryPointsInfo(implicit ec: ExecutionContext): Future[EntryPointsInfo] =
    tree.map(EntryPointsInfo.forClassDef)
}

object MemClassDefIRFile {
  def apply(classDef: ClassDef): IRFile =
    new MemClassDefIRFile(classDef)
}
