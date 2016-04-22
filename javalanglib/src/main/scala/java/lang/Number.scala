package java.lang

import scala.scalajs.js

abstract class Number extends Object with java.io.Serializable {
  def byteValue(): scala.Byte = intValue.toByte
  def shortValue(): scala.Short = intValue.toShort
  def intValue(): scala.Int
  def longValue(): scala.Long
  def floatValue(): scala.Float
  def doubleValue(): scala.Double
}
