package org.scalajs.core.tools.json

import org.scalajs.core.tools.io.IO

import scala.scalajs.js

import java.io.{Writer, Reader}

private[json] object Impl extends AbstractJSONImpl {

  type Repr = js.Any

  def fromString(x: String): Repr = x
  def fromNumber(x: Number): Repr = x.doubleValue()
  def fromBoolean(x: Boolean): Repr = x
  def fromList(x: List[Repr]): Repr = js.Array(x: _*)
  def fromMap(x: Map[String, Repr]): Repr = js.Dictionary(x.toSeq: _*)

  def toString(x: Repr): String = x.asInstanceOf[String]
  def toNumber(x: Repr): Number = x.asInstanceOf[Double]
  def toBoolean(x: Repr): Boolean = x.asInstanceOf[Boolean]
  def toList(x: Repr): List[Repr] = x.asInstanceOf[js.Array[Repr]].toList
  def toMap(x: Repr): Map[String, Repr] =
    x.asInstanceOf[js.Dictionary[Repr]].toMap

  def serialize(x: Repr): String = js.JSON.stringify(x)

  def serialize(x: Repr, writer: Writer): Unit =
    writer.write(serialize(x))

  def deserialize(str: String): Repr = js.JSON.parse(str)

  def deserialize(reader: Reader): Repr =
    deserialize(IO.readReaderToString(reader))

}
