/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.scalajs.reflect

import java.util.Map.Entry
import java.util.AbstractMap.SimpleImmutableEntry
import java.util.function.Function

import org.scalajs.javalibintf.{Reflect => IntfReflect}

import scala.scalajs.js

final class LoadableModuleClass private[reflect] (
    underlying: IntfReflect.LoadableModuleClass[_]) {

  val runtimeClass: Class[_] = underlying.getRuntimeClass()

  /** Loads the module instance and returns it. */
  def loadModule(): Any = underlying.loadModule()
}

final class InstantiatableClass private[reflect] (
    underlying: IntfReflect.InstantiatableClass[_]
) {
  val runtimeClass: Class[_] = underlying.getRuntimeClass()

  val declaredConstructors: List[InvokableConstructor] =
    underlying.getDeclaredConstructors().toList.map(new InvokableConstructor(_))

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

final class InvokableConstructor private[reflect] (
    underlying: IntfReflect.InvokableConstructor[_]
) {
  val parameterTypes: List[Class[_]] = underlying.getParameterTypes().toList

  def newInstance(args: Any*): Any =
    underlying.newInstance(args.asInstanceOf[Seq[Object]]: _*)
}

object Reflect {
  @deprecated("used only by deprecated code", since = "1.20.0")
  @js.native
  private trait JSFunctionVarArgs[T] extends js.Function {
    def apply(args: Object*): T
  }

  /* `protected[reflect]` makes these methods public in the IR.
   *
   * These methods were part of the "public ABI" used by the compiler codegen
   * before Scala.js 1.20. We must preserve backward binary compatibility for
   * them, like for public methods.
   */

  @deprecated(
      "use org.scalajs.javalibintf.Reflect.registerLoadableModuleClass instead",
      since = "1.20.0")
  protected[reflect] def registerLoadableModuleClass[T](
      fqcn: String, runtimeClass: Class[T],
      loadModuleFun: js.Function0[T]): Unit = {
    IntfReflect.registerLoadableModuleClass(fqcn, runtimeClass,
        () => loadModuleFun())
  }

  @deprecated(
      "use org.scalajs.javalibintf.Reflect.registerLoadableModuleClass instead",
      since = "1.20.0")
  protected[reflect] def registerInstantiatableClass[T](
      fqcn: String, runtimeClass: Class[T],
      constructors: js.Array[js.Tuple2[js.Array[Class[_]], js.Function]]): Unit = {

    type EntryKey = Array[Class[_]]
    type EntryValue = Function[Array[Object], T]

    val entries: Array[Entry[EntryKey, EntryValue]] = constructors.map { c =>
      new SimpleImmutableEntry[EntryKey, EntryValue](
          c._1.toArray,
          args => c._2.asInstanceOf[JSFunctionVarArgs[T]].apply(args: _*))
    }.toArray

    IntfReflect.registerInstantiatableClass(fqcn, runtimeClass, entries)
  }

  /** Reflectively looks up a loadable module class.
   *
   *  A module class is the technical term referring to the class of a Scala
   *  `object`. The object or one of its super types (classes or traits) must
   *  be annotated with
   *  [[scala.scalajs.reflect.annotation.EnableReflectiveInstantiation @EnableReflectiveInstantiation]].
   *  Moreover, the object must be "static", i.e., declared at the top-level of
   *  a package or inside a static object.
   *
   *  If the module class cannot be found, either because it does not exist,
   *  was not `@EnableReflectiveInstantiation` or was not static, this method
   *  returns `None`.
   *
   *  @param fqcn
   *    Fully-qualified name of the module class, including its trailing `$`
   */
  def lookupLoadableModuleClass(fqcn: String): Option[LoadableModuleClass] = {
    Option(IntfReflect.lookupLoadableModuleClass(fqcn).orElse(null))
      .map(new LoadableModuleClass(_))
  }

  /** Reflectively looks up an instantiable class.
   *
   *  The class or one of its super types (classes or traits) must be annotated
   *  with
   *  [[scala.scalajs.reflect.annotation.EnableReflectiveInstantiation @EnableReflectiveInstantiation]].
   *  Moreover, the class must not be abstract, nor be a local class (i.e., a
   *  class defined inside a `def`). Inner classes (defined inside another
   *  class) are supported.
   *
   *  If the class cannot be found, either because it does not exist,
   *  was not `@EnableReflectiveInstantiation` or was abstract or local, this
   *  method returns `None`.
   *
   *  @param fqcn
   *    Fully-qualified name of the class
   */
  def lookupInstantiatableClass(fqcn: String): Option[InstantiatableClass] = {
    Option(IntfReflect.lookupInstantiatableClass(fqcn).orElse(null))
      .map(new InstantiatableClass(_))
  }
}
