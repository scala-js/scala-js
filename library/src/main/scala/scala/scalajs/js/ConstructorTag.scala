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

import scala.scalajs.js

/** Stores the JS constructor function of a JS class.
 *
 *  A `ConstructorTag[T]` holds the constructor function of a JS class, as
 *  retrieved by `js.constructorOf[T]`. Similarly to
 *  [[scala.reflect.ClassTag ClassTag]]s, `ConstructorTag`s can be implicitly
 *  materialized when `T` is statically known to be a JS class, i.e., a valid
 *  type argument to `js.constructorOf`.
 */
final class ConstructorTag[T <: js.Any] private[scalajs] (
    val constructor: js.Dynamic) // intentionally public
    extends AnyVal {

  /** Instantiates the class `T` with the specified arguments.
   *
   *  Note that, unlike [[js.Dynamic.newInstance]], this method accepts
   *  `scala.Any`s as parameters.
   */
  def newInstance(args: scala.Any*): T = {
    js.Dynamic.newInstance(constructor)(
        args.asInstanceOf[Seq[js.Any]]: _*).asInstanceOf[T]
  }
}

object ConstructorTag {

  /** Implicitly materializes a [[js.ConstructorTag]].
   *
   *  This method has the same preconditions as [[js.constructorOf]].
   */
  implicit def materialize[T <: js.Any]: js.ConstructorTag[T] =
    throw new java.lang.Error("stub")
}
