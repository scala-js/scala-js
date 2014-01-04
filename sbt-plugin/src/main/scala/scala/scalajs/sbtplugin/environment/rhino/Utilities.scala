/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.environment.rhino

import scala.annotation.tailrec

import org.mozilla.javascript
import org.mozilla.javascript.Context
import org.mozilla.javascript.NativeObject
import org.mozilla.javascript.Scriptable
import org.mozilla.javascript.Scriptable.NOT_FOUND
import org.mozilla.javascript.ScriptableObject

trait Utilities { self: CodeBlock =>

  def getObject(base: String, className: String) = {
    val classNameKey = className.replaceAll("\\.", "_")

    val classObject =
      getFrom(scope, s"$base.$classNameKey".split("\\."), scope)

    classObject match {
      case Right(classObject: javascript.Function) =>
        classObject
      case Right(noFunction) =>
        throw new RuntimeException(
            s"Could not find constructor function, type of element was: ${noFunction.getClass.getName}")
      case Left(key) =>
        throw new RuntimeException(
            s"Could not find $className, undefined key: $key")
    }
  }

  def toArgs(args: Seq[Any]) = args.map(Context.javaToJS(_, scope)).toArray

  def createInstance(className: String, args: Any*) = {
    val classObject = getObject("ScalaJS.classes", className)
    classObject.construct(context, scope, toArgs(args))
  }

  def getModule(className: String) = {
    val moduleObject = getObject("ScalaJS.modules", className)
    moduleObject.call(context, scope, scope, Array.empty).asInstanceOf[NativeObject]
  }

  def callMethod(obj: NativeObject, methodName: String, args: Any*) = {
    val method = ScriptableObject.getProperty(obj, methodName)

    method match {
      case NOT_FOUND =>
        throw new RuntimeException(s"Could not find method $methodName")
      case method: javascript.Function =>
        method.call(context, scope, obj, toArgs(args))
      case other =>
        throw new RuntimeException(
            s"$methodName is not a method, type was: ${other.getClass.getName}")
    }
  }

  @tailrec
  private def getFrom(obj: Scriptable, keys: Seq[String],
      scope: Scriptable): Either[String, Scriptable] = {
    if (keys.isEmpty) Right(obj)
    else {
      val key = keys.head
      val result = obj.get(key, scope)
      if (result == NOT_FOUND) Left(key)
      else getFrom(result.asInstanceOf[Scriptable], keys.tail, scope)
    }
  }
}
