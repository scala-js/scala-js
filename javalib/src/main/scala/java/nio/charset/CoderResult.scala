package java.nio.charset

import scala.annotation.switch

import scala.collection.mutable

import java.nio._

class CoderResult private (kind: Int, _length: Int) {
  import CoderResult._

  @inline def isUnderflow(): Boolean  = kind == Underflow
  @inline def isOverflow(): Boolean   = kind == Overflow
  @inline def isMalformed(): Boolean  = kind == Malformed
  @inline def isUnmappable(): Boolean = kind == Unmappable

  @inline def isError(): Boolean = isMalformed || isUnmappable

  @inline def length(): Int = {
    val l = _length
    if (l < 0)
      throw new UnsupportedOperationException
    l
  }

  def throwException(): Unit = (kind: @switch) match {
    case Overflow   => throw new BufferOverflowException
    case Underflow  => throw new BufferUnderflowException
    case Malformed  => throw new MalformedInputException(_length)
    case Unmappable => throw new UnmappableCharacterException(_length)
  }
}

object CoderResult {
  private final val Underflow = 0
  private final val Overflow = 1
  private final val Malformed = 2
  private final val Unmappable = 3

  val OVERFLOW: CoderResult = new CoderResult(Overflow, -1)
  val UNDERFLOW: CoderResult = new CoderResult(Underflow, -1)

  private val Malformed1 = new CoderResult(Malformed, 1)
  private val Malformed2 = new CoderResult(Malformed, 2)
  private val Malformed3 = new CoderResult(Malformed, 3)
  private val Malformed4 = new CoderResult(Malformed, 4)

  private val uniqueMalformed = mutable.Map.empty[Int, CoderResult]

  private val Unmappable1 = new CoderResult(Unmappable, 1)
  private val Unmappable2 = new CoderResult(Unmappable, 2)
  private val Unmappable3 = new CoderResult(Unmappable, 3)
  private val Unmappable4 = new CoderResult(Unmappable, 4)

  private val uniqueUnmappable = mutable.Map.empty[Int, CoderResult]

  @inline def malformedForLength(length: Int): CoderResult = (length: @switch) match {
    case 1 => Malformed1
    case 2 => Malformed2
    case 3 => Malformed3
    case 4 => Malformed4
    case _ => malformedForLengthImpl(length)
  }

  private def malformedForLengthImpl(length: Int): CoderResult =
    uniqueMalformed.getOrElseUpdate(length, new CoderResult(Malformed, length))

  @inline def unmappableForLength(length: Int): CoderResult = (length: @switch) match {
    case 1 => Unmappable1
    case 2 => Unmappable2
    case 3 => Unmappable3
    case 4 => Unmappable4
    case _ => unmappableForLengthImpl(length)
  }

  private def unmappableForLengthImpl(length: Int): CoderResult =
    uniqueUnmappable.getOrElseUpdate(length, new CoderResult(Unmappable, length))
}
