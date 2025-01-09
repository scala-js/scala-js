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

package org.scalajs.linker.checker

import org.scalajs.ir.Trees.Transient

import org.scalajs.linker.checker.CheckingPhase._
import org.scalajs.linker.frontend.Desugarer.{Transients => DesugarerTransientNodes}

/** A set of conditional IR features that the checkers can accept.
 *
 *  At any given phase, the `ClassDefChecker` and the `IRChecker` must agree on
 *  the set of IR features that are valid. A `FeatureSet` factors out the
 *  knowledge of what feature is acceptable when.
 */
private[checker] final class FeatureSet private (private val flags: Int) extends AnyVal {
  /** Does this feature set support (all of) the given feature set. */
  def supports(features: FeatureSet): Boolean =
    (features.flags & flags) == features.flags

  /** The union of this feature set and `that` feature set. */
  def |(that: FeatureSet): FeatureSet =
    new FeatureSet(this.flags | that.flags)
}

private[checker] object FeatureSet {
  /** Empty feature set. */
  val Empty = new FeatureSet(0)

  // Individual features

  /** The `LinkTimeProperty` IR node. */
  val LinkTimeProperty = new FeatureSet(1 << 0)

  /** Optional constructors in module classes and JS classes. */
  val OptionalConstructors = new FeatureSet(1 << 1)

  /** Reflective proxy definitions. */
  val ReflectiveProxies = new FeatureSet(1 << 2)

  /** The `Desugarer.Transients.Desugar` transient. */
  val DesugarTransient = new FeatureSet(1 << 3)

  /** Transients that are the result of optimizations. */
  val OptimizedTransients = new FeatureSet(1 << 4)

  /** Records and record types. */
  val Records = new FeatureSet(1 << 5)

  /** Relaxed constructor discipline.
   *
   *  - Optional super/delegate constructor call.
   *  - Delegate constructor calls can target any super class.
   *  - `this.x = ...` assignments before the delegate call can assign super class fields.
   *  - `StoreModule` can be anywhere, or not be there at all.
   */
  val RelaxedCtorBodies = new FeatureSet(1 << 6)

  // Common sets

  /** Features introduced by the base linker, except those consumed by desugaring. */
  private val Linked =
    OptionalConstructors | ReflectiveProxies

  /** Features that must be desugared away. */
  private val NeedsDesugaring =
    LinkTimeProperty

  /** IR that is only the result of desugaring (currently empty). */
  private val Desugared =
    Empty

  /** IR that is only the result of optimizations. */
  private val Optimized =
    OptimizedTransients | Records | RelaxedCtorBodies

  /** The set of features supported (as input) by the given phase. */
  def supportedBy(phase: CheckingPhase): FeatureSet = phase match {
    case BaseLinker                => NeedsDesugaring
    case Desugarer                 => Linked | NeedsDesugaring | DesugarTransient
    case Emitter(false)            => Linked | Desugared
    case Emitter(true) | Optimizer => Linked | Desugared | Optimized
  }

  /** A feature set representing a particular transient.
   *
   *  We are not very specific here. By default, we consider that the transient
   *  is the result of optimizations.
   */
  def ofTransient(transientValue: Transient.Value): FeatureSet = transientValue match {
    case _: DesugarerTransientNodes.Desugar =>
      DesugarTransient
    case _ =>
      OptimizedTransients
  }
}
