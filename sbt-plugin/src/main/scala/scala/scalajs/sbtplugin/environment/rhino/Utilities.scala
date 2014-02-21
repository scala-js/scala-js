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

  private def getObject(base: String, className: String) = {
    val classNameKey = className.replaceAll("\\.", "_")

    val classObject =
      getFrom(scope, s"$base.$classNameKey".split("\\."), scope)

    classObject match {
      case Right(classObject) =>
        classObject
      case Left(key) =>
        throw new RuntimeException(
            s"Could not find $className, undefined key: $key")
    }
  }

  private def getFunctionObject(base: String, className: String) = {
    getObject(base, className) match {
      case fun: javascript.Function => fun
      case noFunction =>
        throw new RuntimeException(
            s"Could not find constructor function, type of element was: ${noFunction.getClass.getName}")
    }
  }

  def toArgs(args: Seq[Any]) = args.map(Context.javaToJS(_, scope)).toArray

  def createInstance(className: String, initName: String)(args: Any*) = {
    val classObject = getFunctionObject("ScalaJS.c", className)
    val inst = classObject.construct(context, scope, Array())
    val method =
      ScriptableObject.getProperty(inst, initName).asInstanceOf[javascript.Function]

    method.call(context, scope, inst, toArgs(args))
  }

  def getModule(className: String) = {
    val moduleObject = getFunctionObject("ScalaJS.modules", className)
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

  def callMainMethod(obj: NativeObject, args: Array[String]): Unit =
    callMethod(obj, "main__AT__V", toScalaJSArray(args))

  def callMainMethod(moduleName: String, args: Array[String]): Unit =
    callMainMethod(getModule(moduleName), args)

  def toScalaJSArray[A](array: Array[A]): Scriptable = {
    val result = createScalaJSArray(
        array.getClass.getComponentType.getName, array.length)

    val underlying = ScriptableObject.getProperty(
        result, "underlying").asInstanceOf[Scriptable]
    for (i <- 0 until array.length) {
      val item = Context.javaToJS(array(i), scope)
      ScriptableObject.putProperty(underlying, i, item)
    }

    result
  }

  private def createScalaJSArray(componentClassName: String,
      length: Int): Scriptable = {
    val elementClassData = getObject("ScalaJS.data", componentClassName)
    val arrayClassData = ScriptableObject.callMethod(
        context, elementClassData, "getArrayOf", Array()).asInstanceOf[Scriptable]
    val constr = ScriptableObject.getProperty(
        arrayClassData, "constr").asInstanceOf[javascript.Function]
    constr.construct(context, scope, Array(new Integer(length)))
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
