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

/* !!!!!
 * THIS FILE IS ALMOST COPY-PASTED IN javalanglib/ AND javalib/.
 * THEY MUST BE KEPT IN SYNC.
 *
 * This file acts as a bridge for java.lang.Math to be able to access the
 * additional methods introduced in JDK 8 from javalib/ even when compiling
 * under JDK 6 or 7.
 *
 * In javalib/, this file has only the signatures, with stub implementations.
 * In javalanglib/, the methods delegate to the corresponding methods in
 * java.lang.Math.
 * The build only keeps the .sjsir file from javalanglib/.
 * This way, we create a bridge between javalanglib and javalib. But that
 * means the binary interface of MathJDK8Bridge must be strictly equivalent in
 * the two copies.
 *
 * (Yes, this is a hack.)
 * !!!!!
 */

package java.lang

private[java] object MathJDK8Bridge {
  def addExact(a: scala.Int, b: scala.Int): scala.Int = stub()

  def addExact(a: scala.Long, b: scala.Long): scala.Long = stub()

  def subtractExact(a: scala.Int, b: scala.Int): scala.Int = stub()

  def subtractExact(a: scala.Long, b: scala.Long): scala.Long = stub()

  def multiplyExact(a: scala.Int, b: scala.Int): scala.Int = stub()

  def multiplyExact(a: scala.Long, b: scala.Long): scala.Long = stub()

  def incrementExact(a: scala.Int): scala.Int = stub()

  def incrementExact(a: scala.Long): scala.Long = stub()

  def decrementExact(a: scala.Int): scala.Int = stub()

  def decrementExact(a: scala.Long): scala.Long = stub()

  def negateExact(a: scala.Int): scala.Int = stub()

  def negateExact(a: scala.Long): scala.Long = stub()

  def toIntExact(a: scala.Long): scala.Int = stub()

  def floorDiv(a: scala.Int, b: scala.Int): scala.Int = stub()

  def floorDiv(a: scala.Long, b: scala.Long): scala.Long = stub()

  def floorMod(a: scala.Int, b: scala.Int): scala.Int = stub()

  def floorMod(a: scala.Long, b: scala.Long): scala.Long = stub()

  // A few other Math-related methods that are not really in Math

  def toUnsignedString(i: scala.Int): String = stub()

  def toUnsignedString(i: scala.Long): String = stub()

  def divideUnsigned(a: scala.Int, b: scala.Int): scala.Int = stub()

  def divideUnsigned(a: scala.Long, b: scala.Long): scala.Long = stub()

  def remainderUnsigned(a: scala.Int, b: scala.Int): scala.Int = stub()

  def remainderUnsigned(a: scala.Long, b: scala.Long): scala.Long = stub()

  private def stub(): Nothing =
    throw new Error("stub")
}
