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

package org.scalajs.core.tools.json

import scala.collection.mutable

class JSONObjExtractor(rawData: JSON) {
  private val data = Impl.toMap(rawData)

  def fld[T: JSONDeserializer](name: String): T =
    fromJSON[T](data(name))

  def opt[T: JSONDeserializer](name: String): Option[T] =
    data.get(name).map(fromJSON[T] _)
}
