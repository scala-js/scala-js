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

import scala.collection.mutable

trait KnowledgeAccessor extends Cache {
  private val _registeredTo = mutable.HashSet.empty[KnowledgeSource[_, _]]

  private[caching] def registeredTo(source: KnowledgeSource[_, _]): Unit =
    _registeredTo += source

  override def invalidate(): Unit = {
    super.invalidate()
    _registeredTo.foreach(_.unregister(this))
    _registeredTo.clear()
  }
}
