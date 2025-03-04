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

package org.scalajs.ir

class InvalidIRException(val optTree: Option[Trees.IRNode], message: String)
    extends Exception(message) {

  def this(tree: Trees.IRNode, message: String) =
    this(Some(tree), message)

  def this(message: String) =
    this(None, message)
}
