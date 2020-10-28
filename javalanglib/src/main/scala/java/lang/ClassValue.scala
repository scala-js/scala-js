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
import scala.scalajs.runtime.linkingInfo

import Utils._

abstract class ClassValue[T] protected () {
  private val jsMap: js.Map[Class[_], T] = {
    if (linkingInfo.assumingES6 || js.typeOf(js.Dynamic.global.Map) != "undefined")
      new js.Map()
    else
      null
  }

  @inline
  private def useJSMap: scala.Boolean = {
    /* The linking-info test allows to constant-fold this method as `true` when
     * emitting ES 2015 code, which allows to dead-code-eliminate the branches
     * using `HashMap`s, and therefore `HashMap` itself.
     */
    linkingInfo.assumingES6 || jsMap != null
  }

  /* We use a HashMap instead of an IdentityHashMap because the latter is
   * implemented in terms of the former anyway, to a HashMap is leaner and
   * faster.
   */
  private val javaMap: HashMap[Class[_], T] =
    if (useJSMap) null
    else new HashMap()

  protected def computeValue(`type`: Class[_]): T

  def get(`type`: Class[_]): T = {
    /* We first perform `get`, and if the result is undefined/null, we use
     * `has` to disambiguate a present undefined/null from an absent key.
     * Since the purpose of ClassValue is to be used a cache indexed by Class
     * values, the expected use case will have more hits than misses, and so
     * this ordering should be faster on average than first performing `has`
     * then `get`.
     */
    if (useJSMap) {
      undefOrGetOrElseCompute(mapGet(jsMap, `type`)) { () =>
        if (mapHas(jsMap, `type`)) {
          ().asInstanceOf[T]
        } else {
          val newValue = computeValue(`type`)
          mapSet(jsMap, `type`, newValue)
          newValue
        }
      }
    } else {
      javaMap.get(`type`) match {
        case null =>
          if (javaMap.containsKey(`type`)) {
            null.asInstanceOf[T]
          } else {
            val newValue = computeValue(`type`)
            javaMap.put(`type`, newValue)
            newValue
          }
        case value =>
          value
      }
    }
  }

  def remove(`type`: Class[_]): Unit = {
    if (useJSMap)
      jsMap.delete(`type`)
    else
      javaMap.remove(`type`)
  }
}
