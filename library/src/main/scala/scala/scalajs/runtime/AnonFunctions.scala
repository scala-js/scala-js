package scala.scalajs.runtime

import scala.scalajs.js
import scala.runtime._

// scalastyle:off line.size.limit

@inline
final class AnonFunction0[+R](f: js.Function0[R]) extends AbstractFunction0[R] {
  override def apply(): R = f()
}

@inline
final class AnonFunction1[-T1, +R](f: js.Function1[T1, R]) extends AbstractFunction1[T1, R] {
  override def apply(arg1: T1): R = f(arg1)
}

@inline
final class AnonFunction2[-T1, -T2, +R](f: js.Function2[T1, T2, R]) extends AbstractFunction2[T1, T2, R] {
  override def apply(arg1: T1, arg2: T2): R = f(arg1, arg2)
}

@inline
final class AnonFunction3[-T1, -T2, -T3, +R](f: js.Function3[T1, T2, T3, R]) extends AbstractFunction3[T1, T2, T3, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3): R = f(arg1, arg2, arg3)
}

@inline
final class AnonFunction4[-T1, -T2, -T3, -T4, +R](f: js.Function4[T1, T2, T3, T4, R]) extends AbstractFunction4[T1, T2, T3, T4, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4): R = f(arg1, arg2, arg3, arg4)
}

@inline
final class AnonFunction5[-T1, -T2, -T3, -T4, -T5, +R](f: js.Function5[T1, T2, T3, T4, T5, R]) extends AbstractFunction5[T1, T2, T3, T4, T5, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5): R = f(arg1, arg2, arg3, arg4, arg5)
}

@inline
final class AnonFunction6[-T1, -T2, -T3, -T4, -T5, -T6, +R](f: js.Function6[T1, T2, T3, T4, T5, T6, R]) extends AbstractFunction6[T1, T2, T3, T4, T5, T6, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6): R = f(arg1, arg2, arg3, arg4, arg5, arg6)
}

@inline
final class AnonFunction7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R](f: js.Function7[T1, T2, T3, T4, T5, T6, T7, R]) extends AbstractFunction7[T1, T2, T3, T4, T5, T6, T7, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
}

@inline
final class AnonFunction8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R](f: js.Function8[T1, T2, T3, T4, T5, T6, T7, T8, R]) extends AbstractFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
}

@inline
final class AnonFunction9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R](f: js.Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) extends AbstractFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
}

@inline
final class AnonFunction10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R](f: js.Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]) extends AbstractFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
}

@inline
final class AnonFunction11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R](f: js.Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]) extends AbstractFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
}

@inline
final class AnonFunction12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R](f: js.Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]) extends AbstractFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
}

@inline
final class AnonFunction13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R](f: js.Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) extends AbstractFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
}

@inline
final class AnonFunction14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R](f: js.Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]) extends AbstractFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
}

@inline
final class AnonFunction15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R](f: js.Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]) extends AbstractFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
}

@inline
final class AnonFunction16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R](f: js.Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) extends AbstractFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
}

@inline
final class AnonFunction17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R](f: js.Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]) extends AbstractFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
}

@inline
final class AnonFunction18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R](f: js.Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]) extends AbstractFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
}

@inline
final class AnonFunction19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R](f: js.Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]) extends AbstractFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
}

@inline
final class AnonFunction20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R](f: js.Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]) extends AbstractFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
}

@inline
final class AnonFunction21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R](f: js.Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]) extends AbstractFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20, arg21: T21): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21)
}

@inline
final class AnonFunction22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R](f: js.Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]) extends AbstractFunction22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] {
  override def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20, arg21: T21, arg22: T22): R = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22)
}

// scalastyle:on line.size.limit
