package scala.scalajs.js.runtime

import scala.scalajs.js
import scala.runtime._

final class AnonFunction0[+R](f: js.Function0[R]) extends AbstractFunction0[R] {
  override def apply(): R = f()
}

final class AnonFunction1[-T1, +R](f: js.Function1[T1, R]) extends AbstractFunction1[T1, R] {
  override def apply(arg1: T1): R = f(arg1)
}

final class AnonFunction2[-T1, -T2, +R](f: js.Function2[T1, T2, R]) extends AbstractFunction2[T1, T2, R] {
  override def apply(arg1: T1, arg2: T2): R = f(arg1, arg2)
}

final class AnonFunction3[-T1, -T2, -T3, +R](f: js.Function3[T1, T2, T3, R]) extends AbstractFunction3[T1, T2, T3, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3): R = f(arg1, arg2, arg3)
}

final class AnonFunction4[-T1, -T2, -T3, -T4, +R](f: js.Function4[T1, T2, T3, T4, R]) extends AbstractFunction4[T1, T2, T3, T4, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4): R = f(arg1, arg2, arg3, arg4)
}

final class AnonFunction5[-T1, -T2, -T3, -T4, -T5, +R](f: js.Function5[T1, T2, T3, T4, T5, R]) extends AbstractFunction5[T1, T2, T3, T4, T5, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5): R = f(arg1, arg2, arg3, arg4, arg5)
}
