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
import scala.runtime._

// scalastyle:off line.size.limit

/* Before 1.19, these classes were concrete. They had a 1-argument constructor
 * taking a js.FunctionN, and their `apply()` method called that function.
 *
 * In 1.19, we introduced `NewLambda` nodes, which superseded these specialized
 * classes with a compilation mode that is more efficient on Wasm.
 * However, libraries compiled with earlier versions still contain references
 * to `AnonFunctionN`.
 *
 * The IR deserializer patches allocations of the form
 *   New(AnonFunctionN, ctor, closure :: Nil)
 * into
 *   NewLambda(AnonFunctionN, ..., (...xs) => closure(...xs))
 *
 * When the `closure` is directly a JS `Closure` with the right number of
 * arguments (which is supposed to be always, as far as our codegens were
 * concerned), it rewrites that as
 *   NewLambda(AnonFunctionN, ..., (...closureParams) => closureBody)
 * which provides the best performance for old code.
 */

// scalafmt: { maxColumn = 1000 }

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction0[+R] extends AbstractFunction0[R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction1[-T1, +R] extends AbstractFunction1[T1, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction2[-T1, -T2, +R] extends AbstractFunction2[T1, T2, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction3[-T1, -T2, -T3, +R] extends AbstractFunction3[T1, T2, T3, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction4[-T1, -T2, -T3, -T4, +R] extends AbstractFunction4[T1, T2, T3, T4, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction5[-T1, -T2, -T3, -T4, -T5, +R] extends AbstractFunction5[T1, T2, T3, T4, T5, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends AbstractFunction6[T1, T2, T3, T4, T5, T6, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends AbstractFunction7[T1, T2, T3, T4, T5, T6, T7, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends AbstractFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends AbstractFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends AbstractFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R] extends AbstractFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R] extends AbstractFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R] extends AbstractFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R] extends AbstractFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R] extends AbstractFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R] extends AbstractFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R] extends AbstractFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R] extends AbstractFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R] extends AbstractFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R] extends AbstractFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R] extends AbstractFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]

@deprecated("used by the codegen before 1.19", since = "1.19.0")
sealed abstract class AnonFunction22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R] extends AbstractFunction22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]

// scalafmt: {}
// scalastyle:on line.size.limit
