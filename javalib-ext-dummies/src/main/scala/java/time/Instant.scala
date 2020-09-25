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

package java.time

final class Instant(private val epochSecond: Long, private val nano: Int) {
  def getEpochSecond(): Long = epochSecond

  def getNano(): Int = nano

  def toEpochMilli(): Long = {
    if (epochSecond == -9223372036854776L) {
      /* Special case: epochSecond * 1000L would overflow, but the addition
       * of the nanos might save the day. So we transfer one unit of the
       * seconds into the contribution of the nanos.
       */
      Math.addExact(-9223372036854775000L, (nano / 1000000) - 1000)
    } else {
      Math.addExact(Math.multiplyExact(epochSecond, 1000L), nano / 1000000)
    }
  }

  def isAfter(otherInstant: Instant): Boolean = {
    this.epochSecond > otherInstant.epochSecond || {
      this.epochSecond == otherInstant.epochSecond &&
      this.nano > otherInstant.nano
    }
  }

  def isBefore(otherInstant: Instant): Boolean = {
    this.epochSecond < otherInstant.epochSecond || {
      this.epochSecond == otherInstant.epochSecond &&
      this.nano < otherInstant.nano
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: Instant =>
      this.epochSecond == that.epochSecond &&
      this.nano == that.nano
    case _ =>
      false
  }

  override def hashCode(): Int =
    java.lang.Long.hashCode(epochSecond) ^ java.lang.Integer.hashCode(nano)

  // non compliant, for debugging purposes only
  override def toString(): String =
    "Instant(" + epochSecond + ", " + nano + ")"
}

object Instant {
  final val MIN: Instant = new Instant(-31557014167219200L, 0)
  final val MAX: Instant = new Instant(31556889864403199L, 999999999)

  private def checkAndCreate(epochSecond: Long, nano: Int): Instant = {
    val instant = new Instant(epochSecond, nano)
    if (instant.isBefore(MIN) || instant.isAfter(MAX))
      throw new DateTimeException("Instant exceeds minimum or maximum instant")
    instant
  }

  def ofEpochSecond(epochSecond: Long, nanoAdjustment: Long): Instant = {
    val adjustedSecond =
      Math.addExact(epochSecond, Math.floorDiv(nanoAdjustment, 1000000000L))
    val adjustedNano = Math.floorMod(nanoAdjustment, 1000000000L).toInt
    checkAndCreate(adjustedSecond, adjustedNano)
  }

  def ofEpochMilli(epochMilli: Long): Instant = {
    new Instant(Math.floorDiv(epochMilli, 1000L),
        1000000 * Math.floorMod(epochMilli, 1000L).toInt)
  }
}
