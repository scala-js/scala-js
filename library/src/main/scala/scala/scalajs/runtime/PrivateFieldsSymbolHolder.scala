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
  val privateFieldsSymbol: Any = {
    // Cannot import scala.scalajs.LinkingInfo because it is shadowed by runtime.LinkingInfo
    if (scala.scalajs.LinkingInfo.esVersion >= ESVersion.ES2015 ||
        js.typeOf(js.Symbol) != "undefined") {
      js.Symbol("privateFields")
    } else {
      def rand32(): String = {
        val s = {
          ((js.Math.random() * 4294967296.0).asInstanceOf[js.Dynamic] >>> 0.asInstanceOf[js.Dynamic])
            .applyDynamic("toString")(16).asInstanceOf[String]
        }
        "00000000".jsSubstring(s.length) + s
      }
      rand32() + rand32() + rand32() + rand32()
    }
  }
}
