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

import java.io.IOException

import org.scalajs.linker.interface.{IRContainer, IRFile}

/** A virtual file containing Scala.js IR.
 *
 *  The main difference compared to using individual files
 *  (that are extracted beforehand) is that the fileset can be versioned at a
 *  higher level: the container needs to change its version when any of the
 *  files change. Therefore, the entire extraction process can be cached.
 */
abstract class IRContainerImpl(
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
) extends IRContainer {
  private[interface] final def impl: IRContainerImpl = this

  /** All the `*.sjsir` files in this container.
   *
   *  It is up to the implementation whether these files are read lazily or not.
   */
  def sjsirFiles(implicit ec: ExecutionContext): Future[List[IRFile]]
}


object IRContainerImpl {
  def fromIRContainer(c: IRContainer): IRContainerImpl = c.impl
}
