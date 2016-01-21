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
private[rhino] class LazyScalaJSScope(
    coreLib: ScalaJSCoreLib,
    globalScope: Scriptable,
    base: Scriptable,
    isStatics: Boolean) extends Scriptable {

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

  override def getClassName(): String = "LazyScalaJSScope"

  override def get(name: String, start: Scriptable): AnyRef = {
    if (name == "__noSuchMethod__") {
      /* Automatically called by Rhino when trying to call a method fails.
       * We don't want to throw a ClassNotFoundException for this case, but
       * rather return a proper NOT_FOUND sentinel. Otherwise, this exception
       * would "shadow" the real one containing the class name that could not
       * be found on the classpath.
       */
      Scriptable.NOT_FOUND
    } else {
      fields.getOrElse(name, {
        try {
          load(name)
          fields.getOrElse(name, Scriptable.NOT_FOUND)
        } catch {
          // We need to re-throw the exception if `load` fails, otherwise the
          // JavaScript runtime will not catch it.
          case t: RhinoJSEnv.ClassNotFoundException =>
            throw Context.throwAsScriptRuntimeEx(t)
        }
      }).asInstanceOf[AnyRef]
    }
  }

  override def get(index: Int, start: Scriptable): AnyRef =
    get(index.toString, start)

  override def has(name: String, start: Scriptable): Boolean =
    fields.contains(name)
  override def has(index: Int, start: Scriptable): Boolean =
    has(index.toString, start)

  override def put(name: String, start: Scriptable, value: Any): Unit =
    fields(name) = value
  override def put(index: Int, start: Scriptable, value: Any): Unit =
    put(index.toString, start, value)

  override def delete(name: String): Unit = ()
  override def delete(index: Int): Unit = ()

  override def getPrototype(): Scriptable = prototype
  override def setPrototype(value: Scriptable): Unit = prototype = value

  override def getParentScope(): Scriptable = parentScope
  override def setParentScope(value: Scriptable): Unit = parentScope = value

  override def getIds(): Array[AnyRef] = fields.keys.toArray

  override def getDefaultValue(hint: java.lang.Class[_]): AnyRef = {
    base.getDefaultValue(hint)
  }

  override def hasInstance(instance: Scriptable): Boolean = false
}
