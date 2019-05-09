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

package java.util

import java.{util => ju}

import scala.collection.mutable

import ScalaOps._

class Properties(protected val defaults: Properties)
    extends ju.Hashtable[AnyRef, AnyRef] {

  def this() = this(null)

  def setProperty(key: String, value: String): AnyRef =
    put(key, value)

  // def load(reader: Reader): Unit
  // def load(inStream: InputStream): Unit
  // @deprecated("", "") def save(out: OutputStream, comments: String): Unit
  // def store(writer: Writer, comments: String): Unit
  // def store(out: OutputStream, comments: String): Unit
  // def loadFromXML(in: InputStream): Unit
  // def storeToXML(os: OutputStream, comment: String): Unit
  // def storeToXML(os: OutputStream, comment: String, encoding: String): Unit

  def getProperty(key: String): String =
    getProperty(key, defaultValue = null)

  def getProperty(key: String, defaultValue: String): String = {
    get(key) match {
      case value: String => value

      case _ =>
        if (defaults != null) defaults.getProperty(key, defaultValue)
        else defaultValue
    }
  }

  def propertyNames(): ju.Enumeration[_] = {
    val propNames = mutable.Set.empty[Any]
    // Explicitly use asInstanceOf, to trigger the ClassCastException mandated by the spec
    keySet().scalaOps.foreach(propNames += _.asInstanceOf[String])
    if (defaults != null)
      defaults.propertyNames().scalaOps.foreach(propNames += _)
    propNames.iterator.asJavaEnumeration
  }

  def stringPropertyNames(): ju.Set[String] = {
    val set = new ju.HashSet[String]
    entrySet().scalaOps.foreach { entry =>
      (entry.getKey, entry.getValue) match {
        case (key: String, _: String) => set.add(key)
        case _                        => // Ignore key
      }
    }
    if (defaults != null)
      set.addAll(defaults.stringPropertyNames())
    set
  }

  // def list(out: PrintStream): Unit
  // def list(out: PrintWriter): Unit
}
