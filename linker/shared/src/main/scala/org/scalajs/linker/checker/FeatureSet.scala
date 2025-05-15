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

import org.scalajs.linker.checker.CheckingPhase._

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

  /** Link-time IR nodes: `LinkTimeProperty` and `LinkTimeIf`. */
  val LinkTimeNodes = new FeatureSet(1 << 0)

  /** The `NewLambda` IR node. */
  val NewLambda = new FeatureSet(1 << 1)

  /** Optional constructors in module classes and JS classes. */
  val OptionalConstructors = new FeatureSet(1 << 2)

  /** Explicit reflective proxy definitions. */
  val ReflectiveProxies = new FeatureSet(1 << 3)

  /** `TransientTypeRef`s. */
  val TransientTypeRefs = new FeatureSet(1 << 4)

  /** General typed closures (not only in `NewLambda` nodes). */
  val TypedClosures = new FeatureSet(1 << 5)

  /** Transients that are the result of optimizations. */
  val OptimizedTransients = new FeatureSet(1 << 6)

  /** Records and record types. */
  val Records = new FeatureSet(1 << 7)

  /** Relaxed constructor discipline.
   *
   *  - Optional super/delegate constructor call.
   *  - Delegate constructor calls can target any super class.
   *  - `this.x = ...` assignments before the delegate call can assign super class fields.
   *  - `StoreModule` can be anywhere, or not be there at all.
   */
  val RelaxedCtorBodies = new FeatureSet(1 << 8)

  // Common sets

  /** Features introduced by the base linker.
   *
   *  Although `NewLambda` nodes themselves are desugared in the `Desugarer`,
   *  the corresponding synthetic *classes* already have an existence after the
   *  `BaseLinker`. They must, since they must participate in the CHA
   *  performed by the `Analyzer`. So `TransientTypeRef`s and `TypedClosure`s
   *  can already appear after the `BaseLinker`.
   */
  private val Linked =
    OptionalConstructors | ReflectiveProxies | TransientTypeRefs | TypedClosures

  /** Features that must be desugared away. */
  private val NeedsDesugaring =
    LinkTimeNodes | NewLambda

  /** IR that is only the result of desugaring (currently empty). */
  private val Desugared =
    Empty

  /** IR that is only the result of optimizations. */
  private val Optimized =
    OptimizedTransients | Records | RelaxedCtorBodies

  /** The set of features allowed as output of the given phase. */
  def allowedAfter(phase: CheckingPhase): FeatureSet = phase match {
    case Compiler   => NeedsDesugaring
    case BaseLinker => Linked | NeedsDesugaring
    case Desugarer  => Linked | Desugared
    case Optimizer  => Linked | Desugared | Optimized
  }
}
