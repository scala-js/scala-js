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

package org.scalajs.linker

import scala.concurrent._

import org.scalajs.linker.standard._

abstract class IRContainer private[linker] () {
  private[linker] def impl: IRContainerImpl
}

object IRContainer {
  def fromIRFile(irFile: IRFile): IRContainer = {
    val f = IRFileImpl.fromIRFile(irFile)
    new IRContainerImpl(f.path, f.version) {
      def sjsirFiles(implicit ec: ExecutionContext): Future[List[IRFile]] =
        Future.successful(List(irFile))
    }
  }
}
