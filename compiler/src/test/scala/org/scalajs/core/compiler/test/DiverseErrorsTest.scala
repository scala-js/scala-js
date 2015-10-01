package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._
import org.junit.Test

// scalastyle:off line.size.limit

class DiverseErrorsTest extends DirectTest with TestHelpers  {

  override def preamble: String =
    """import scala.scalajs.js, js.annotation._
    """

  @Test
  def noIsInstanceOnJSRaw: Unit = {

    """
    @js.native
    trait JSRaw extends js.Object

    class A {
      val a: AnyRef = "asdf"
      def x = a.isInstanceOf[JSRaw]
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: isInstanceOf[JSRaw] not supported because it is a raw JS trait
      |      def x = a.isInstanceOf[JSRaw]
      |                            ^
    """

  }

  @Test
  def jsConstructorOfErrors: Unit = {

    """
    class ScalaClass
    trait ScalaTrait
    object ScalaObject

    object A {
      val a = js.constructorOf[ScalaClass]
      val b = js.constructorOf[ScalaTrait]
      val c = js.constructorOf[ScalaObject.type]
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: type arguments [ScalaClass] do not conform to method constructorOf's type parameter bounds [T <: scala.scalajs.js.Any]
      |      val a = js.constructorOf[ScalaClass]
      |                              ^
      |newSource1.scala:9: error: type arguments [ScalaTrait] do not conform to method constructorOf's type parameter bounds [T <: scala.scalajs.js.Any]
      |      val b = js.constructorOf[ScalaTrait]
      |                              ^
      |newSource1.scala:10: error: type arguments [ScalaObject.type] do not conform to method constructorOf's type parameter bounds [T <: scala.scalajs.js.Any]
      |      val c = js.constructorOf[ScalaObject.type]
      |                              ^
    """

    """
    @js.native class NativeJSClass extends js.Object
    @js.native trait NativeJSTrait extends js.Object
    @js.native object NativeJSObject extends js.Object

    @ScalaJSDefined class JSClass extends js.Object
    @ScalaJSDefined trait JSTrait extends js.Object
    @ScalaJSDefined object JSObject extends js.Object

    object A {
      val a = js.constructorOf[NativeJSTrait]
      val b = js.constructorOf[NativeJSObject.type]

      val c = js.constructorOf[NativeJSClass with NativeJSTrait]
      val d = js.constructorOf[NativeJSClass { def bar: Int }]

      val e = js.constructorOf[JSTrait]
      val f = js.constructorOf[JSObject.type]

      val g = js.constructorOf[JSClass with JSTrait]
      val h = js.constructorOf[JSClass { def bar: Int }]

      def foo[A <: js.Any] = js.constructorOf[A]
      def bar[A <: js.Any: scala.reflect.ClassTag] = js.constructorOf[A]
    }
    """ hasErrors
    """
      |newSource1.scala:12: error: non-trait class type required but NativeJSTrait found
      |      val a = js.constructorOf[NativeJSTrait]
      |                               ^
      |newSource1.scala:13: error: class type required but NativeJSObject.type found
      |      val b = js.constructorOf[NativeJSObject.type]
      |                                             ^
      |newSource1.scala:15: error: class type required but NativeJSClass with NativeJSTrait found
      |      val c = js.constructorOf[NativeJSClass with NativeJSTrait]
      |                               ^
      |newSource1.scala:16: error: class type required but NativeJSClass{def bar: Int} found
      |      val d = js.constructorOf[NativeJSClass { def bar: Int }]
      |                               ^
      |newSource1.scala:18: error: non-trait class type required but JSTrait found
      |      val e = js.constructorOf[JSTrait]
      |                               ^
      |newSource1.scala:19: error: class type required but JSObject.type found
      |      val f = js.constructorOf[JSObject.type]
      |                                       ^
      |newSource1.scala:21: error: class type required but JSClass with JSTrait found
      |      val g = js.constructorOf[JSClass with JSTrait]
      |                               ^
      |newSource1.scala:22: error: class type required but JSClass{def bar: Int} found
      |      val h = js.constructorOf[JSClass { def bar: Int }]
      |                               ^
      |newSource1.scala:24: error: class type required but A found
      |      def foo[A <: js.Any] = js.constructorOf[A]
      |                                              ^
      |newSource1.scala:25: error: class type required but A found
      |      def bar[A <: js.Any: scala.reflect.ClassTag] = js.constructorOf[A]
      |                                                                      ^
    """

  }

  @Test
  def jsConstructorTagErrors: Unit = {

    """
    class ScalaClass
    trait ScalaTrait
    object ScalaObject

    object A {
      val a = js.constructorTag[ScalaClass]
      val b = js.constructorTag[ScalaTrait]
      val c = js.constructorTag[ScalaObject.type]
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: type arguments [ScalaClass] do not conform to method constructorTag's type parameter bounds [T <: scala.scalajs.js.Any]
      |      val a = js.constructorTag[ScalaClass]
      |                               ^
      |newSource1.scala:9: error: type arguments [ScalaTrait] do not conform to method constructorTag's type parameter bounds [T <: scala.scalajs.js.Any]
      |      val b = js.constructorTag[ScalaTrait]
      |                               ^
      |newSource1.scala:10: error: type arguments [ScalaObject.type] do not conform to method constructorTag's type parameter bounds [T <: scala.scalajs.js.Any]
      |      val c = js.constructorTag[ScalaObject.type]
      |                               ^
    """

    """
    @js.native class NativeJSClass extends js.Object
    @js.native trait NativeJSTrait extends js.Object
    @js.native object NativeJSObject extends js.Object

    @ScalaJSDefined class JSClass extends js.Object
    @ScalaJSDefined trait JSTrait extends js.Object
    @ScalaJSDefined object JSObject extends js.Object

    object A {
      val a = js.constructorTag[NativeJSTrait]
      val b = js.constructorTag[NativeJSObject.type]

      val c = js.constructorTag[NativeJSClass with NativeJSTrait]
      val d = js.constructorTag[NativeJSClass { def bar: Int }]

      val e = js.constructorTag[JSTrait]
      val f = js.constructorTag[JSObject.type]

      val g = js.constructorTag[JSClass with JSTrait]
      val h = js.constructorTag[JSClass { def bar: Int }]

      def foo[A <: js.Any] = js.constructorTag[A]
      def bar[A <: js.Any: scala.reflect.ClassTag] = js.constructorTag[A]
    }
    """ hasErrors
    """
      |newSource1.scala:12: error: non-trait class type required but NativeJSTrait found
      |      val a = js.constructorTag[NativeJSTrait]
      |                               ^
      |newSource1.scala:13: error: class type required but NativeJSObject.type found
      |      val b = js.constructorTag[NativeJSObject.type]
      |                               ^
      |newSource1.scala:15: error: class type required but NativeJSClass with NativeJSTrait found
      |      val c = js.constructorTag[NativeJSClass with NativeJSTrait]
      |                               ^
      |newSource1.scala:16: error: class type required but NativeJSClass{def bar: Int} found
      |      val d = js.constructorTag[NativeJSClass { def bar: Int }]
      |                               ^
      |newSource1.scala:18: error: non-trait class type required but JSTrait found
      |      val e = js.constructorTag[JSTrait]
      |                               ^
      |newSource1.scala:19: error: class type required but JSObject.type found
      |      val f = js.constructorTag[JSObject.type]
      |                               ^
      |newSource1.scala:21: error: class type required but JSClass with JSTrait found
      |      val g = js.constructorTag[JSClass with JSTrait]
      |                               ^
      |newSource1.scala:22: error: class type required but JSClass{def bar: Int} found
      |      val h = js.constructorTag[JSClass { def bar: Int }]
      |                               ^
      |newSource1.scala:24: error: class type required but A found
      |      def foo[A <: js.Any] = js.constructorTag[A]
      |                                              ^
      |newSource1.scala:25: error: class type required but A found
      |      def bar[A <: js.Any: scala.reflect.ClassTag] = js.constructorTag[A]
      |                                                                      ^
    """

  }

  @Test
  def runtimeConstructorOfErrors: Unit = {

    """
    import scala.scalajs.runtime

    object ScalaObject
    @js.native object NativeJSObject extends js.Object
    @ScalaJSDefined object JSObject extends js.Object

    object A {
      val a = runtime.constructorOf(classOf[ScalaObject.type].asInstanceOf[Class[_ <: js.Any]])
      val b = runtime.constructorOf(classOf[NativeJSObject.type])
      val c = runtime.constructorOf(classOf[JSObject.type])
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: class type required but ScalaObject.type found
      |      val a = runtime.constructorOf(classOf[ScalaObject.type].asInstanceOf[Class[_ <: js.Any]])
      |                                                       ^
      |newSource1.scala:11: error: class type required but NativeJSObject.type found
      |      val b = runtime.constructorOf(classOf[NativeJSObject.type])
      |                                                          ^
      |newSource1.scala:12: error: class type required but JSObject.type found
      |      val c = runtime.constructorOf(classOf[JSObject.type])
      |                                                    ^
    """

    """
    import scala.scalajs.runtime

    class ScalaClass
    trait ScalaTrait

    @js.native class NativeJSClass extends js.Object
    @js.native trait NativeJSTrait extends js.Object
    @js.native object NativeJSObject extends js.Object

    @ScalaJSDefined class JSClass extends js.Object
    @ScalaJSDefined trait JSTrait extends js.Object
    @ScalaJSDefined object JSObject extends js.Object

    object A {
      val a = runtime.constructorOf(classOf[ScalaClass].asInstanceOf[Class[_ <: js.Any]])
      val b = runtime.constructorOf(classOf[ScalaTrait].asInstanceOf[Class[_ <: js.Any]])

      val c = runtime.constructorOf(classOf[NativeJSTrait])
      val d = runtime.constructorOf(classOf[JSTrait])

      def jsClassClass = classOf[JSClass]
      val e = runtime.constructorOf(jsClassClass)

      val f = runtime.constructorOf(NativeJSObject.getClass)
      val g = runtime.constructorOf(JSObject.getClass)
    }
    """ hasErrors
    """
      |newSource1.scala:17: error: runtime.constructorOf() must be called with a constant classOf[T] representing a class extending js.Any (not a trait nor an object)
      |      val a = runtime.constructorOf(classOf[ScalaClass].asInstanceOf[Class[_ <: js.Any]])
      |                                   ^
      |newSource1.scala:18: error: runtime.constructorOf() must be called with a constant classOf[T] representing a class extending js.Any (not a trait nor an object)
      |      val b = runtime.constructorOf(classOf[ScalaTrait].asInstanceOf[Class[_ <: js.Any]])
      |                                   ^
      |newSource1.scala:20: error: runtime.constructorOf() must be called with a constant classOf[T] representing a class extending js.Any (not a trait nor an object)
      |      val c = runtime.constructorOf(classOf[NativeJSTrait])
      |                                   ^
      |newSource1.scala:21: error: runtime.constructorOf() must be called with a constant classOf[T] representing a class extending js.Any (not a trait nor an object)
      |      val d = runtime.constructorOf(classOf[JSTrait])
      |                                   ^
      |newSource1.scala:24: error: runtime.constructorOf() must be called with a constant classOf[T] representing a class extending js.Any (not a trait nor an object)
      |      val e = runtime.constructorOf(jsClassClass)
      |                                   ^
      |newSource1.scala:26: error: runtime.constructorOf() must be called with a constant classOf[T] representing a class extending js.Any (not a trait nor an object)
      |      val f = runtime.constructorOf(NativeJSObject.getClass)
      |                                   ^
      |newSource1.scala:27: error: runtime.constructorOf() must be called with a constant classOf[T] representing a class extending js.Any (not a trait nor an object)
      |      val g = runtime.constructorOf(JSObject.getClass)
      |                                   ^
    """

  }

}
