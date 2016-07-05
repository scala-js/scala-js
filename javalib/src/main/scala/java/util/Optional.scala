package java.util

final class Optional[T] private (value: T) {

  def get(): T = {
    if (!isPresent())
      throw new NoSuchElementException()
    else
      value
  }

  def isPresent(): Boolean = value != null

  //def ifPresent(consumer: Consumer[_ >: T]): Unit
  //def filter(predicate: Predicate[_ >: T]): Optional[U]
  //def map[U](mapper: Function[_ >: T, _ <: U]): Optional[U]
  //def flatMap[U](mapper: Function[_ >: T, Optional[U]]): Optional[U]

  def orElse(other: T): T = {
    if (isPresent) value
    else other
  }

  //def orElseGet(other: Supplier[_ <: T]): T
  //def orElseThrow[X](exceptionSupplier: Supplier[_ <: X]): T

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
}
