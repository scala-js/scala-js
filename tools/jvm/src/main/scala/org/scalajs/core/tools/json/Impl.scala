package org.scalajs.core.tools.json

import org.json.simple.JSONValue

import scala.collection.JavaConverters._

import java.io.{Writer, Reader}

private[json] object Impl extends AbstractJSONImpl {

  type Repr = Object

  def fromString(x: String): Repr = x
  def fromNumber(x: Number): Repr = x
  def fromBoolean(x: Boolean): Repr = java.lang.Boolean.valueOf(x)
  def fromList(x: List[Repr]): Repr = x.asJava
  def fromMap(x: Map[String, Repr]): Repr = x.asJava

  def toString(x: Repr): String = x.asInstanceOf[String]
  def toNumber(x: Repr): Number = x.asInstanceOf[Number]
  def toBoolean(x: Repr): Boolean =
    x.asInstanceOf[java.lang.Boolean].booleanValue()
  def toList(x: Repr): List[Repr] =
    x.asInstanceOf[java.util.List[Repr]].asScala.toList
  def toMap(x: Repr): Map[String, Repr] =
    x.asInstanceOf[java.util.Map[String, Repr]].asScala.toMap

  def serialize(x: Repr): String =
    JSONValue.toJSONString(x)

  def serialize(x: Repr, writer: Writer): Unit =
    JSONValue.writeJSONString(x, writer)

  def deserialize(str: String): Repr = JSONValue.parse(str)

  def deserialize(reader: Reader): Repr = JSONValue.parse(reader)

}
