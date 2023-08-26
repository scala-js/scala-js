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

package org.scalajs.linker.analyzer

import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext

import java.util.concurrent.Executors

private[analyzer] object Platform {
  def emptyThreadSafeMap[K, V]: mutable.Map[K, V] = TrieMap.empty

  def adjustExecutionContextForParallelism(ec: ExecutionContext,
      parallel: Boolean): ExecutionContext = {
    /* Parallel is the default. Parallelism is disabled (likely for debugging),
     * we create our own single thread executor. This is for sure not the most
     * efficient, but it is simpler than, say, attempting to build single thread
     * execution on top of an arbitrary execution context.
     * Further, if parallel is false, we do not expect that speed is the primary
     * aim of the execution.
     */
    if (parallel) ec
    else ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
  }
}
