package scala.scalajs.sbtplugin.environment.rhino

import org.mozilla.javascript.Scriptable
import java.io.File
import org.mozilla.javascript.Context
import scala.collection.mutable

/**
 * A proxy for a ScalaJS "scope" field that loads scripts lazily
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
case class LazyScalaJSScope(
  providers: scala.collection.Map[String, File],
  globalScope: Scriptable,
  base: Scriptable,
  isModule: Boolean = false,
  isTraitImpl: Boolean = false)
  extends Scriptable {

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

  private def load(name: String): Unit = {
    val relativeFileName = nameToRelativeFileName(name)
    providers.get(relativeFileName) foreach { file =>
      val ctx = Context.getCurrentContext()
      ctx.evaluateFile(globalScope, file)
    }
  }

  private def nameToRelativeFileName(name: String): String = {
    val name1 = if (isTraitImpl) name.split("__")(0) else name
    val name2 = name1.replace("_", "/").replace("$und", "_")
    if (isModule) name2 + "$.js"
    else name2 + ".js"
  }

  override def getClassName() = "LazyScalaJSScope"

  override def get(name: String, start: Scriptable) = {
    fields.getOrElse(name, {
      load(name)
      fields.getOrElse(name, Scriptable.NOT_FOUND)
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
