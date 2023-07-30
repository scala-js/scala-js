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

private[analyzer] object Platform {
  def emptyThreadSafeMap[K, V]: mutable.Map[K, V] = TrieMap.empty
}
