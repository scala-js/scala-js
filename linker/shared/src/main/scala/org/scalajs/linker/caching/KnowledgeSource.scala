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

abstract class KnowledgeSource[I, A](initInput: I) {
  private var knowledge = compute(initInput)
  private val askers = mutable.HashSet.empty[KnowledgeAccessor]

  private[caching] def unregister(accessor: KnowledgeAccessor): Unit =
    askers -= accessor

  protected def compute(input: I): A

  def update(input: I): Unit = {
    val newKnowledge = compute(input)
    if (!sameKnowledge(newKnowledge, knowledge)) {
      knowledge = newKnowledge
      invalidateAskers()
    }
  }

  protected def sameKnowledge(a: A, b: A): Boolean =
    a == b

  private def invalidateAskers(): Unit = {
    /* Calling `invalidate` causes the `KnowledgeAccessor` to call
     * `unregister()` in this class, which will mutate the `askers` set.
     * Therefore, we cannot directly iterate over `askers`, and need to take a
     * snapshot instead.
     */
    val snapshot = askers.toSeq
    askers.clear()
    snapshot.foreach(_.invalidate())
  }

  def askKnowledge(accessor: KnowledgeAccessor): A = {
    if (askers.add(accessor))
      accessor.registeredTo(this)
    knowledge
  }
}

object KnowledgeSource {
  def apply[I, A](initInput: I)(computeFun: I => A): KnowledgeSource[I, A] = {
    new KnowledgeSource[I, A](initInput) {
      protected def compute(input: I): A =
        computeFun(input)
    }
  }

  def apply[I1, I2, A](initInput1: I1, initInput2: I2)(
      computeFun: (I1, I2) => A): KnowledgeSource[(I1, I2), A] = {
    new KnowledgeSource[(I1, I2), A]((initInput1, initInput2)) {
      protected def compute(input: (I1, I2)): A =
        computeFun(input._1, input._2)
    }
  }

  def apply[I1, I2, I3, A](initInput1: I1, initInput2: I2, initInput3: I3)(
      computeFun: (I1, I2, I3) => A): KnowledgeSource[(I1, I2, I3), A] = {
    new KnowledgeSource[(I1, I2, I3), A]((initInput1, initInput2, initInput3)) {
      protected def compute(input: (I1, I2, I3)): A =
        computeFun(input._1, input._2, input._3)
    }
  }

  def withCustomComparison[I, A](initInput: I)(computeFun: I => A)(
      sameKnowledgeFun: (A, A) => Boolean): KnowledgeSource[I, A] = {
    new KnowledgeSource[I, A](initInput) {
      protected def compute(input: I): A =
        computeFun(input)

      override protected def sameKnowledge(a: A, b: A): Boolean =
        sameKnowledgeFun(a, b)
    }
  }
}
