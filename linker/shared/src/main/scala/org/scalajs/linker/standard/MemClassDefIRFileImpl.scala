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

import scala.concurrent._

import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.Trees.ClassDef
import org.scalajs.ir.Version

import org.scalajs.linker.interface.unstable.IRFileImpl

/** A simple in-memory virtual Scala.js IR file with a ClassDef. */
final class MemClassDefIRFileImpl(
    path: String,
    version: Version,
    classDef: ClassDef
) extends IRFileImpl(path, version) {
  private val _entryPointsInfo = EntryPointsInfo.forClassDef(classDef)

  def entryPointsInfo(implicit ec: ExecutionContext): Future[EntryPointsInfo] =
    Future.successful(_entryPointsInfo)

  def tree(implicit ec: ExecutionContext): Future[ClassDef] =
    Future.successful(classDef)
}
