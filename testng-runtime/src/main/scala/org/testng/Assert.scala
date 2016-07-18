package org.testng

import java.lang.Iterable
import java.util.{Collection, Iterator}

object Assert {
  def assertEquals[T          ](actual:            T , expected:            T                                 ): Unit = if (actual != expected) throw new AssertionError()
  def assertEquals[T          ](actual:            T , expected:            T ,                message: String): Unit = if (actual != expected) throw new AssertionError(message)
  def assertEquals[T <: AnyRef](actual:      Array[T], expected:      Array[T]                                ): Unit = if (actual.sameElements(expected)) throw new AssertionError()
  def assertEquals[T <: AnyRef](actual:      Array[T], expected:      Array[T],                message: String): Unit = if (actual.sameElements(expected)) throw new AssertionError(message)
  def assertEquals[T          ](actual: Collection[T], expected: Collection[T]                                ): Unit = ()
  def assertEquals[T          ](actual: Collection[T], expected: Collection[T],                message: String): Unit = ()
  def assertEquals[T          ](actual:   Iterable[T], expected:   Iterable[T]                                ): Unit = ()
  def assertEquals[T          ](actual:   Iterable[T], expected:   Iterable[T],                message: String): Unit = ()
  def assertEquals[T          ](actual:   Iterator[T], expected:   Iterator[T]                                ): Unit = ()
  def assertEquals[T          ](actual:   Iterator[T], expected:   Iterator[T],                message: String): Unit = ()

  def assertEquals[T          ](actual:       Double , expected:       Double , delta: Double                 ): Unit = ()
  def assertEquals[T          ](actual:       Double , expected:       Double , delta: Double, message: String): Unit = ()
  def assertEquals[T          ](actual:        Float , expected:        Float , delta: Float                  ): Unit = ()
  def assertEquals[T          ](actual:        Float , expected:        Float , delta: Float , message: String): Unit = ()

  def assertTrue               (condition: Boolean,                                            message: String = null): Unit = ()
}
