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

package org.scalajs.linker.interface

sealed abstract class CheckedBehavior {
  import CheckedBehavior._
  def optimized: CheckedBehavior = this match {
    case Fatal => Unchecked
    case _     => this
  }
}

object CheckedBehavior {
  case object Compliant extends CheckedBehavior
  case object Fatal extends CheckedBehavior
  case object Unchecked extends CheckedBehavior

  private[interface] implicit object CheckedBehaviorFingerprint
      extends Fingerprint[CheckedBehavior] {

    override def fingerprint(behavior: CheckedBehavior): String = {
      behavior match {
        case Compliant => "Compliant"
        case Fatal     => "Fatal"
        case Unchecked => "Unchecked"
      }
    }
  }
}
