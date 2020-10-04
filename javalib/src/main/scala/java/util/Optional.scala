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

package java.util

import java.util.function._

final class Optional[T] private (value: T) {
  import Optional._

  def get(): T = {
    if (!isPresent())
      throw new NoSuchElementException()
    else
      value
  }

  @inline def isPresent(): Boolean = value != null

  @inline def isEmpty(): Boolean = value == null

  def ifPresent(action: Consumer[_ >: T]): Unit = {
    if (isPresent())
      action.accept(value)
  }

  def ifPresentOrElse(action: Consumer[_ >: T], emptyAction: Runnable): Unit = {
    if (isPresent())
      action.accept(value)
    else
      emptyAction.run()
  }

  def filter(predicate: Predicate[_ >: T]): Optional[T] =
    if (isEmpty() || predicate.test(value)) this
    else Optional.empty()

  def map[U](mapper: Function[_ >: T, _ <: U]): Optional[U] =
    if (isEmpty()) emptyCast[U](this)
    else Optional.ofNullable(mapper(value))

  def flatMap[U](mapper: Function[_ >: T, Optional[_ <: U]]): Optional[U] =
    if (isEmpty()) emptyCast[U](this)
    else upcast[U](mapper(value))

  def or(supplier: Supplier[_ <: Optional[_ <: T]]): Optional[T] =
    if (isPresent()) this
    else upcast[T](supplier.get())

  def orElse(other: T): T =
    if (isPresent()) value
    else other

  def orElseGet(supplier: Supplier[_ <: T]): T =
    if (isPresent()) value
    else supplier.get()

  def orElseThrow(): T =
    if (isPresent()) value
    else throw new NoSuchElementException()

  def orElseThrow[X <: Throwable](exceptionSupplier: Supplier[_ <: X]): T =
    if (isPresent()) value
    else throw exceptionSupplier.get()

  override def equals(obj: Any): Boolean = {
    obj match {
      case opt: Optional[_] =>
        (!isPresent() && !opt.isPresent()) ||
        (isPresent() && opt.isPresent() && value.equals(opt.get()))
      case _ => false
    }
  }

  override def hashCode(): Int = {
    if (!isPresent()) 0
    else value.hashCode()
  }

  override def toString(): String = {
    if (!isPresent()) "Optional.empty"
    else s"Optional[$value]"
  }
}

object Optional {
  def empty[T](): Optional[T] = new Optional[T](null.asInstanceOf[T])

  def of[T](value: T): Optional[T] = {
    if (value == null)
      throw new NullPointerException()
    else
      new Optional[T](value)
  }

  def ofNullable[T](value: T): Optional[T] = new Optional[T](value)

  @inline
  private def upcast[T](optional: Optional[_ <: T]): Optional[T] =
    optional.asInstanceOf[Optional[T]]

  @inline
  private def emptyCast[T](empty: Optional[_]): Optional[T] =
    empty.asInstanceOf[Optional[T]]
}
