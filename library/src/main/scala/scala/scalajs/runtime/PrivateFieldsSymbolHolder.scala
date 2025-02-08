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

package scala.scalajs.runtime

import scala.scalajs.js
import scala.scalajs.js.JSStringOps._
import scala.scalajs.LinkingInfo.ESVersion

private[runtime] object PrivateFieldsSymbolHolder {
  val privateFieldsSymbol: Any = js.Symbol("privateFields")
}
