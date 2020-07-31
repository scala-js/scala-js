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

package scala.scalajs.js

import scala.language.implicitConversions
import scala.language.higherKinds

import scala.scalajs.js

/** Value of type A or B (union type).
 *
 *  Scala does not have union types, but they are important to many
 *  interoperability scenarios. This type provides a (partial) encoding of
 *  union types using implicit evidences.
 */
sealed trait |[A, B] // scalastyle:ignore

object | { // scalastyle:ignore
  /** Evidence that `A <: B`, taking `|`-types into account. */
  sealed trait Evidence[-A, +B]

  /** A unique (and typically dead-code-eliminated away) instance of
   *  `Evidence`.
   */
  private object ReusableEvidence extends Evidence[scala.Any, scala.Any]

  abstract sealed class EvidenceLowestPrioImplicits { this: Evidence.type =>
    /** If `A <: B2`, then `A <: B1 | B2`. */
    implicit def right[A, B1, B2](implicit ev: Evidence[A, B2]): Evidence[A, B1 | B2] =
      ReusableEvidence.asInstanceOf[Evidence[A, B1 | B2]]

    /** Given a covariant type constructor `F[+_]`, if `A <: B`, then
     *  `F[A] <: F[B]`.
     */
    implicit def covariant[F[+_], A, B](implicit ev: Evidence[A, B]): Evidence[F[A], F[B]] =
      ReusableEvidence.asInstanceOf[Evidence[F[A], F[B]]]

    /** Given a contravariant type constructor `F[-_]`, if `B <: A`, then
     *  `F[A] <: F[B]`.
     */
    implicit def contravariant[F[-_], A, B](implicit ev: Evidence[B, A]): Evidence[F[A], F[B]] =
      ReusableEvidence.asInstanceOf[Evidence[F[A], F[B]]]
  }

  abstract sealed class EvidenceLowPrioImplicits
      extends EvidenceLowestPrioImplicits {
    this: Evidence.type =>

    /** `Int <: Double`, because that's true in Scala.js. */
    implicit def intDouble: Evidence[Int, Double] =
      ReusableEvidence.asInstanceOf[Evidence[Int, Double]]

    /** If `A <: B1`, then `A <: B1 | B2`. */
    implicit def left[A, B1, B2](implicit ev: Evidence[A, B1]): Evidence[A, B1 | B2] =
      ReusableEvidence.asInstanceOf[Evidence[A, B1 | B2]]

    /** Explicit rules for `A <: A | Unit`.
     *
     * This helps type inference in with `js.UndefOr[A]` which is aliased to `A | Unit`.
     */
    implicit def addUnit[A]: Evidence[A, A | Unit] =
      ReusableEvidence.asInstanceOf[Evidence[A,  A | Unit]]
  }

  object Evidence extends EvidenceLowPrioImplicits {
    /** `A <: A`. */
    implicit def base[A]: Evidence[A, A] =
      ReusableEvidence.asInstanceOf[Evidence[A, A]]

    /** If `A1 <: B` and `A2 <: B`, then `A1 | A2 <: B`. */
    implicit def allSubtypes[A1, A2, B](
        implicit ev1: Evidence[A1, B], ev2: Evidence[A2, B]): Evidence[A1 | A2, B] =
      ReusableEvidence.asInstanceOf[Evidence[A1 | A2, B]]
  }

  /** Upcast `A` to `B1 | B2`.
   *
   *  This needs evidence that `A <: B1 | B2`.
   */
  implicit def from[A, B1, B2](a: A)(implicit ev: Evidence[A, B1 | B2]): B1 | B2 =
    a.asInstanceOf[B1 | B2]

  /** Upcast `Nothing | A2` to `B1 | B2`.
   *
   * This is mostly useful to upcast `js.undefined` to `T` where `T` is an alias `type T = js.UndefOr[A]`.
   * In that case, the @uncheckedVariance on `js.UndefOr` is lost at use-site and we need an implicit upcast.
   */
  implicit def fromNothing[A2, B1, B2](a: Nothing | A2)(
      implicit ev: Evidence[A2, B2]): B1 | B2 =
    a.asInstanceOf[B1 | B2]

  /** Upcast `F[A]` to `F[B]`.
   *
   *  This needs evidence that `F[A] <: F[B]`.
   */
  implicit def fromTypeConstructor[F[_], A, B](a: F[A])(
      implicit ev: Evidence[F[A], F[B]]): F[B] =
    a.asInstanceOf[F[B]]

  /** Operations on union types. */
  implicit class UnionOps[A <: _ | _] private[|] (private val self: A)
      extends AnyVal {

    /** Explicitly merge a union type to a supertype (which might not be a
     *  union type itself).
     *
     *  This needs evidence that `A <: B`.
     */
    def merge[B](implicit ev: |.Evidence[A, B]): B =
      self.asInstanceOf[B]
  }

  /** Provides an [[Option]]-like API to [[js.UndefOr]]. */
  implicit def undefOr2ops[A](value: js.UndefOr[A]): js.UndefOrOps[A] =
    new js.UndefOrOps(value)

  /* This one is not very "in the spirit" of the union type, but it used to be
   * available for `js.UndefOr[A]`, so we keep it for backward source
   * compatibility. It is not really harmful, and has some perks in certain
   * interoperability scenarios.
   */
  implicit def undefOr2jsAny[A](value: js.UndefOr[A])(
      implicit ev: A => js.Any): js.Any = {
    value.map(ev).asInstanceOf[js.Any]
  }
}
