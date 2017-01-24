/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.reflect

import scala.collection.mutable

import scala.scalajs.js

final class LoadableModuleClass private[reflect] (
    val runtimeClass: Class[_],
    loadModuleFun: js.Function0[Any]
) {
  /** Loads the module instance and returns it. */
  def loadModule(): Any = loadModuleFun()
}

final class InstantiatableClass private[reflect] (
    val runtimeClass: Class[_],
    val declaredConstructors: List[InvokableConstructor]
) {
  /** Instantiates a new instance of this class using the zero-argument
   *  constructor.
   *
   *  @throws java.lang.InstantiationException (caused by a
   *    `NoSuchMethodException`)
   *    If this class does not have a public zero-argument constructor.
   */
  def newInstance(): Any = {
    getConstructor().fold[Any] {
      throw new InstantiationException(runtimeClass.getName).initCause(
          new NoSuchMethodException(runtimeClass.getName + ".<init>()"))
    } { ctor =>
      ctor.newInstance()
    }
  }

  /** Looks up a public constructor identified by the types of its formal
   *  parameters.
   *
   *  If no such public constructor exists, returns `None`.
   */
  def getConstructor(parameterTypes: Class[_]*): Option[InvokableConstructor] =
    declaredConstructors.find(_.parameterTypes.sameElements(parameterTypes))
}

final class InvokableConstructor private[reflect]  (
    val parameterTypes: List[Class[_]],
    newInstanceFun: js.Function
) {
  def newInstance(args: Any*): Any = {
    /* Check the number of actual arguments. We let the casts and unbox
     * operations inside `newInstanceFun` take care of the rest.
     */
    require(args.size == parameterTypes.size)
    newInstanceFun.asInstanceOf[js.Dynamic].apply(
        args.asInstanceOf[Seq[js.Any]]: _*)
  }
}

object Reflect {
  /* I would like those val's to be `js.Dictionary`'es instead of full-blown
   * Scala Maps. But if I do that, GCC "dead-code"-eliminates away statements
   * that fill `loadableModuleClasses`! We fool it into not making a fool of
   * itself by using more complicated data structures that it does not
   * understand -_-'.
   */

  private val loadableModuleClasses =
    mutable.Map.empty[String, LoadableModuleClass]

  private val instantiatableClasses =
    mutable.Map.empty[String, InstantiatableClass]

  // `protected[reflect]` makes it public in the IR
  protected[reflect] def registerLoadableModuleClass[T](
      fqcn: String, runtimeClass: Class[T],
      loadModuleFun: js.Function0[T]): Unit = {
    loadableModuleClasses(fqcn) =
      new LoadableModuleClass(runtimeClass, loadModuleFun)
  }

  protected[reflect] def registerInstantiatableClass[T](
      fqcn: String, runtimeClass: Class[T],
      constructors: js.Array[js.Tuple2[js.Array[Class[_]], js.Function]]): Unit = {
    val invokableConstructors = constructors.map { c =>
      new InvokableConstructor(c._1.toList, c._2)
    }
    instantiatableClasses(fqcn) =
      new InstantiatableClass(runtimeClass, invokableConstructors.toList)
  }

  def lookupLoadableModuleClass(fqcn: String): Option[LoadableModuleClass] =
    loadableModuleClasses.get(fqcn)

  def lookupInstantiatableClass(fqcn: String): Option[InstantiatableClass] =
    instantiatableClasses.get(fqcn)
}
