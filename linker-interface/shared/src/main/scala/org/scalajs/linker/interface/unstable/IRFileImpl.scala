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

package org.scalajs.linker.interface.unstable

import scala.concurrent._

import org.scalajs.linker.interface.IRFile

/** A virtual Scala.js IR file.
 *  It contains the class info and the IR tree.
 */
abstract class IRFileImpl(
  /** Abstract path of the file.
   *
   *  The path of the file is used for lookup and caching (together with the
   *  version).
   */
  val path: String,

  /** An optional implementation-dependent "version" token.
   *
   *  If non-empty, a different version must be returned when the content
   *  changes. It should be equal if the content has not changed, but it is
   *  not mandatory.
   *  Such a token can be used by caches: the file need not be read and
   *  processed again if its version has not changed.
   */
  val version: Option[String]
) extends IRFile {
  private[interface] final def impl: IRFileImpl = this

  /** Entry points information for this file. */
  def entryPointsInfo(implicit ec: ExecutionContext): Future[IRFileImpl.EntryPointsInfo]

  /** IR Tree of this file. */
  def tree(implicit ec: ExecutionContext): Future[IRFileImpl.ClassDef]
}

object IRFileImpl {
  type EntryPointsInfo
  type ClassDef

  def fromIRFile(irFile: IRFile): IRFileImpl = irFile.impl
}
