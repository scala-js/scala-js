/*
 * Ported by Alistair Johnson from
 * https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/MathContext.java
 * Original license copied below:
 */

/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package java.math

object MathContext {

  val DECIMAL128 = MathContext(34, RoundingMode.HALF_EVEN)

  val DECIMAL32 = MathContext(7, RoundingMode.HALF_EVEN)

  val DECIMAL64 = MathContext(16, RoundingMode.HALF_EVEN)

  val UNLIMITED = MathContext(0, RoundingMode.HALF_UP)

  private def apply(precision: Int, roundingMode: RoundingMode): MathContext =
    new MathContext(precision, roundingMode)

  private def getArgs(s: String): (Int, RoundingMode) = {
    checkNotNull(s, "null string")
    val precisionLength = "precision=".length
    val roundingModeLength = "roundingMode=".length
    val spaceIndex= s.indexOf(' ', precisionLength)

    if (!s.startsWith("precision=") || spaceIndex == -1)
      invalidMathContext("Missing precision", s)

    val precisionString = s.substring(precisionLength, spaceIndex)
    val precision = {
      try {
        java.lang.Integer.parseInt(precisionString)
      } catch {
        case _: NumberFormatException => invalidMathContext("Bad precision", s)
      }
    }

    val roundingModeStrStart = spaceIndex + 1
    if (!s.regionMatches(roundingModeStrStart, "roundingMode=", 0, roundingModeLength))
      invalidMathContext("Missing rounding mode", s)

    val roundingModeStart = roundingModeStrStart + roundingModeLength
    val roundingMode = RoundingMode.valueOf(s.substring(roundingModeStart))

    (precision, roundingMode)
  }

  private def invalidMathContext(reason: String, s: String): Nothing = {
    throw new IllegalArgumentException(reason + ": " + s)
  }

  private def checkNotNull(reference: AnyRef, errorMessage: AnyRef): Unit = {
    if (reference == null)
      throw new NullPointerException(String.valueOf(errorMessage))
  }
}

class MathContext(setPrecision: Int, setRoundingMode: RoundingMode) {

  private[math] val precision = setPrecision

  private[math] val roundingMode = setRoundingMode

  def getPrecision(): Int = precision

  def getRoundingMode(): RoundingMode = roundingMode

  def this(setPrecision: Int) = {
    this(setPrecision, RoundingMode.HALF_UP)
  }

  private def this(args: (Int, RoundingMode)) = {
    this(args._1, args._2)
  }

  def this(s: String) = {
    this(MathContext.getArgs(s))
    checkValid()
  }

  override def equals(x: Any): Boolean = x match {
    case that: MathContext =>
      this.precision == that.precision &&
      this.roundingMode == that.roundingMode
    case _ =>
      false
  }

  override def hashCode(): Int = (precision << 3) | roundingMode.ordinal()

  override def toString(): String =
    "precision=" + precision + " roundingMode=" + roundingMode

  private def checkValid(): Unit = {
    if (precision < 0)
      throw new IllegalArgumentException("Negative precision: " + precision)
    if (roundingMode == null)
      throw new NullPointerException("roundingMode == null")
  }
}
