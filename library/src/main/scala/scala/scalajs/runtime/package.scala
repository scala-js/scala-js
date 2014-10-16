package scala.scalajs

import scala.collection.GenTraversableOnce

package object runtime {

  def wrapJavaScriptException(e: Any): Throwable = e match {
    case e: Throwable => e
    case _            => js.JavaScriptException(e)
  }

  def unwrapJavaScriptException(th: Throwable): Any = th match {
    case js.JavaScriptException(e) => e
    case _                         => th
  }

  @inline final def genTraversableOnce2jsArray[A](
      col: GenTraversableOnce[A]): js.Array[A] = {
    col match {
      case col: js.ArrayOps[A]     => col.result()
      case col: js.WrappedArray[A] => col.array
      case _ =>
        val result = new js.Array[A]
        col.foreach(x => result.push(x))
        result
    }
  }

  /** Instantiates a JS object with variadic arguments to the constructor. */
  def newJSObjectWithVarargs(ctor: js.Dynamic, args: js.Array[_]): js.Any = {
    // Not really "possible" in JavaScript, so we emulate what it would be.
    val c = ((() => ()): js.Function).asInstanceOf[js.Dynamic]
    c.prototype = ctor.prototype
    val instance = js.Dynamic.newInstance(c)()
    val result = ctor.applyDynamic("apply")(instance, args)
    (result: js.Any) match {
      case _:js.prim.Undefined | _:js.prim.Number | _:js.prim.Boolean |
          _:js.prim.String | null =>
        instance
      case _ =>
        result
    }
  }

  /** Information about the environment Scala.js runs in. */
  def environmentInfo: js.Dynamic = sys.error("stub")

}
