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

package java.io

trait DataInput {
  def readBoolean(): Boolean
  def readByte(): Byte
  def readChar(): Char
  def readDouble(): Double
  def readFloat(): Float
  def readFully(b: Array[Byte]): Unit
  def readFully(b: Array[Byte], off: Int, len: Int): Unit
  def readInt(): Int
  def readLine(): String
  def readLong(): Long
  def readShort(): Short
  def readUnsignedByte(): Int
  def readUnsignedShort(): Int
  def readUTF(): String
  def skipBytes(n: Int): Int
}
