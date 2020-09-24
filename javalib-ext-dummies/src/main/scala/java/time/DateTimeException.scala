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

package java.time

class DateTimeException(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {

  def this(message: String) = this(message, null)
}
