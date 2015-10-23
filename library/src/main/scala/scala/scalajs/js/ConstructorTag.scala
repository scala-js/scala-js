/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

/** Stores the JS constructor function of a JS class.
 *
 *  A `ConstructorTag[T]` holds the constructor function of a JS class, as
 *  retrieved by `js.constructorOf[T]`. Similarly to
 *  [[scala.reflect.ClassTag ClassTag]]s, `ConstructorTag`s can be implicitly
 *  materialized when `T` is statically known to be a JS class, i.e., a valid
 *  type argument to `js.constructorOf`.
 */
final class ConstructorTag[T <: Any] private[scalajs] (
    val constructor: Dynamic) extends AnyVal {

  /** Instantiates the class `T` with the specified arguments.
   *
   *  Note that, unlike [[Dynamic.newInstance js.Dynamic.newInstance]], this
   *  method accepts `scala.Any`s as parameters.
   */
  def newInstance(args: scala.Any*): T =
    Dynamic.newInstance(constructor)(args.asInstanceOf[Seq[Any]]: _*).asInstanceOf[T]
}

object ConstructorTag {
  /** Implicitly materializes a [[ConstructorTag]].
   *
   *  This method has the same preconditions as
   *  [[constructorOf js.constructorOf]].
   */
  implicit def materialize[T <: Any]: ConstructorTag[T] = sys.error("stub")
}
