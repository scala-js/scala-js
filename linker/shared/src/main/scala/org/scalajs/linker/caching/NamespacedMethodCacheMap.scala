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

package org.scalajs.linker.caching

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.MemberNamespace

/** A cache map specialized for keys that are pairs `(MemberNamespace, MethodName)`.
 *
 *  This class follows the same contract as [[CacheMap]].
 */
abstract class NamespacedMethodCacheMap[Value <: Cache] extends Cache with CacheAggregate {
  private val _caches: Array[java.util.Map[MethodName, Value]] =
    Array.fill(MemberNamespace.Count)(createUnderlyingHashMap())

  protected def createUnderlyingHashMap(): java.util.Map[MethodName, Value] =
    new java.util.HashMap()

  protected def createValue(methodName: MethodName): Value

  /** Unique instance of the lambda that we pass to `computeIfAbsent`. */
  private val createValueFunction: java.util.function.Function[MethodName, Value] =
    (methodName: MethodName) => createValue(methodName)

  override def invalidate(): Unit = {
    super.invalidate()
    _caches.foreach(_.clear()) // TODO do we need to invalidate all subcaches?
  }

  def get(namespace: MemberNamespace, methodName: MethodName): Value = {
    markUsed()
    val result = _caches(namespace.ordinal).computeIfAbsent(methodName, createValueFunction)
    result.markUsed()
    result
  }

  override def cleanAfterRun(): Boolean = {
    val result = super.cleanAfterRun()
    if (result)
      _caches.foreach(_.entrySet().removeIf(!_.getValue().cleanAfterRun()))
    result
  }
}
