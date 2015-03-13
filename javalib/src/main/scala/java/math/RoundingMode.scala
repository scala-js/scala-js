/*
 *  Ported by Alistair Johnson from https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/RoundingMode.java
 *  Original license copied below:
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

import scala.annotation.switch

final class RoundingMode private (_index: Int, _name: String) {
  def name(): String = _name
  def ordinal(): Int = _index

  override def toString(): String = name()
}

object RoundingMode {

  final val UP = new RoundingMode(BigDecimal.ROUND_UP, "UP")

  final val DOWN = new RoundingMode(BigDecimal.ROUND_DOWN, "DOWN")

  final val CEILING = new RoundingMode(BigDecimal.ROUND_CEILING, "CEILING")

  final val FLOOR = new RoundingMode(BigDecimal.ROUND_FLOOR, "FLOOR")

  final val HALF_UP = new RoundingMode(BigDecimal.ROUND_HALF_UP, "HALF_UP")

  final val HALF_DOWN = new RoundingMode(BigDecimal.ROUND_HALF_DOWN, "HALF_DOWN")

  final val HALF_EVEN = new RoundingMode(BigDecimal.ROUND_HALF_EVEN, "HALF_EVEN")

  final val UNNECESSARY = new RoundingMode(BigDecimal.ROUND_UNNECESSARY, "UNNECESSARY")

  private val _values: Array[RoundingMode] =
    Array(UP, DOWN, CEILING, FLOOR, HALF_UP, HALF_DOWN, HALF_EVEN, UNNECESSARY)

  def values(): Array[RoundingMode] = _values.clone()

  def valueOf(name: String): RoundingMode = {
    _values.find(_.name == name).getOrElse(
        throw new IllegalArgumentException("No enum const RoundingMode." + name))
  }

  def valueOf(mode: Int): RoundingMode = (mode: @switch) match {
    case BigDecimal.ROUND_CEILING     => CEILING
    case BigDecimal.ROUND_DOWN        => DOWN
    case BigDecimal.ROUND_FLOOR       => FLOOR
    case BigDecimal.ROUND_HALF_DOWN   => HALF_DOWN
    case BigDecimal.ROUND_HALF_EVEN   => HALF_EVEN
    case BigDecimal.ROUND_HALF_UP     => HALF_UP
    case BigDecimal.ROUND_UNNECESSARY => UNNECESSARY
    case BigDecimal.ROUND_UP          => UP
    case _                            =>
      throw new IllegalArgumentException("Invalid rounding mode")
  }
}

