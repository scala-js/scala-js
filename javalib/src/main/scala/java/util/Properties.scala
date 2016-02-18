package java.util

import java.{util => ju}

import scala.collection.JavaConversions
import scala.collection.JavaConversions._

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
    val thisSet = keySet()
    val defaultsIterator =
      if (defaults != null) defaults.propertyNames().toIterator
      else scala.collection.Iterator.empty
    val filteredDefaults = defaultsIterator.collect {
      case k: AnyRef if !thisSet(k) => k
    }
    JavaConversions.asJavaEnumeration(thisSet.iterator() ++ filteredDefaults)
  }

  def stringPropertyNames(): ju.Set[String] = {
    val set = new ju.HashSet[String]
    propertyNames().foreach {
      case key: String => set.add(key)
      case _           => // Ignore key
    }
    set
  }

  // def list(out: PrintStream): Unit
  // def list(out: PrintWriter): Unit
}
