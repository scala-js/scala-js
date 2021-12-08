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

package java.util.internal

@inline
private[java] class BooleanRef(var elem: Boolean) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}
private[java] object BooleanRef {
  def create(elem: Boolean): BooleanRef = new BooleanRef(elem)
  def zero(): BooleanRef = new BooleanRef(false)
}

@inline
private[java] class CharRef(var elem: Char) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}
private[java] object CharRef {
  def create(elem: Char): CharRef = new CharRef(elem)
  def zero(): CharRef = new CharRef(0.toChar)
}

@inline
private[java] class ByteRef(var elem: Byte) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}
private[java] object ByteRef {
  def create(elem: Byte): ByteRef = new ByteRef(elem)
  def zero(): ByteRef = new ByteRef(0)
}

@inline
private[java] class ShortRef(var elem: Short) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}
private[java] object ShortRef {
  def create(elem: Short): ShortRef = new ShortRef(elem)
  def zero(): ShortRef = new ShortRef(0)
}

@inline
private[java] class IntRef(var elem: Int) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}
private[java] object IntRef {
  def create(elem: Int): IntRef = new IntRef(elem)
  def zero(): IntRef = new IntRef(0)
}

@inline
private[java] class LongRef(var elem: Long) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}
private[java] object LongRef {
  def create(elem: Long): LongRef = new LongRef(elem)
  def zero(): LongRef = new LongRef(0)
}

@inline
private[java] class FloatRef(var elem: Float) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}
private[java] object FloatRef {
  def create(elem: Float): FloatRef = new FloatRef(elem)
  def zero(): FloatRef = new FloatRef(0)
}

@inline
private[java] class DoubleRef(var elem: Double) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}
private[java] object DoubleRef {
  def create(elem: Double): DoubleRef = new DoubleRef(elem)
  def zero(): DoubleRef = new DoubleRef(0)
}

@inline
private[java] class ObjectRef[A](var elem: A) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}
private[java] object ObjectRef {
  def create[A](elem: A): ObjectRef[A] = new ObjectRef(elem)
  def zero(): ObjectRef[Object] = new ObjectRef(null)
}
