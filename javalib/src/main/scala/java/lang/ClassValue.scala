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

package java.lang

import java.util.HashMap

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.LinkingInfo
import scala.scalajs.LinkingInfo.ESVersion

import Utils._

abstract class ClassValue[T] protected () {
  private val jsMap: js.Map[Class[_], T] = new js.Map()

  protected def computeValue(`type`: Class[_]): T

  def get(`type`: Class[_]): T =
    mapGetOrElseUpdate(jsMap, `type`)(() => computeValue(`type`))

  def remove(`type`: Class[_]): Unit =
    jsMap.delete(`type`)
}
