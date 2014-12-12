/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.rhino

import scala.collection.mutable

import org.mozilla.javascript.{Scriptable, Context}

/** A proxy for a ScalaJS "scope" field that loads scripts lazily
 *
 *  E.g., ScalaJS.c, which is a scope with the Scala.js classes, can be
 *  turned to a LazyScalaJSScope. Upon first access to a field of ScalaJS.c,
 *  say ScalaJS.c.scala_Option, the script defining that particular
 *  field will be loaded.
 *  This is possible because the relative path to the script can be derived
 *  from the name of the property being accessed.
 *
 *  It is immensely useful, because it allows to load lazily only the scripts
 *  that are actually needed.
 */
class LazyScalaJSScope(
    coreLib: ScalaJSCoreLib,
    globalScope: Scriptable,
    base: Scriptable,
    isStatics: Boolean = false) extends Scriptable {

  private val fields = mutable.HashMap.empty[String, Any]
  private var prototype: Scriptable = _
  private var parentScope: Scriptable = _

  {
    // Pre-fill fields with the properties of `base`
    for (id <- base.getIds()) {
      (id.asInstanceOf[Any]: @unchecked) match {
        case name: String => put(name, this, base.get(name, base))
        case index: Int => put(index, this, base.get(index, base))
      }
    }
  }

  private def load(name: String): Unit =
    coreLib.load(globalScope, propNameToEncodedName(name))

  private def propNameToEncodedName(name: String): String = {
    if (isStatics) name.split("__")(0)
    else name
  }

  override def getClassName() = "LazyScalaJSScope"

  override def get(name: String, start: Scriptable) = {
    fields.getOrElse(name, {
      try {
        load(name)
        fields.getOrElse(name, Scriptable.NOT_FOUND)
      } catch {
        // We need to re-throw the exception if `load` fails, otherwise the
        // JavaScript runtime will not catch it.
        case t: ScalaJSCoreLib.ClassNotFoundException =>
          throw Context.throwAsScriptRuntimeEx(t)
      }
    }).asInstanceOf[AnyRef]
  }
  override def get(index: Int, start: Scriptable) =
    get(index.toString, start)

  override def has(name: String, start: Scriptable) =
    fields.contains(name)
  override def has(index: Int, start: Scriptable) =
    has(index.toString, start)

  override def put(name: String, start: Scriptable, value: Any) = {
    fields(name) = value
  }
  override def put(index: Int, start: Scriptable, value: Any) =
    put(index.toString, start, value)

  override def delete(name: String) = ()
  override def delete(index: Int) = ()

  override def getPrototype() = prototype
  override def setPrototype(value: Scriptable) = prototype = value

  override def getParentScope() = parentScope
  override def setParentScope(value: Scriptable) = parentScope = value

  override def getIds() = fields.keys.toArray

  override def getDefaultValue(hint: java.lang.Class[_]) = {
    base.getDefaultValue(hint)
  }

  override def hasInstance(instance: Scriptable) = false
}
