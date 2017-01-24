package org.scalajs.testinterface

import language.experimental.macros

/** Dummy object to get the right shadowing for 2.10 / 2.11 cross compilation */
private object Compat210 {
  object blackbox { // scalastyle:ignore
    type Context = scala.reflect.macros.Context
  }
}

import Compat210._

object TestUtils {
  import scala.reflect.macros._ // shadows blackbox from above
  import blackbox.Context

  @deprecated(
      "Use the overload with explicit formal constructor parameter types.",
      "0.6.15")
  def newInstance(name: String, loader: ClassLoader)(args: Seq[AnyRef]): AnyRef =
    macro newInstance_impl

  def newInstance(name: String, loader: ClassLoader, paramTypes: Seq[Class[_]])(
      args: Seq[AnyRef]): AnyRef =
    macro newInstance_impl2

  def newInstance_impl(c: Context)(name: c.Expr[String],
      loader: c.Expr[ClassLoader])(
      args: c.Expr[Seq[AnyRef]]): c.Expr[AnyRef] = c.universe.reify {

    val clazz = loader.splice.loadClass(name.splice)
    val ctors = clazz.getConstructors()

    if (ctors.size != 1) {
      throw new IllegalArgumentException(
          "You may only call newInstance with single-ctor classes")
    }

    val ctor = ctors.head
    ctor.newInstance(args.splice: _*).asInstanceOf[AnyRef]
  }

  def newInstance_impl2(c: Context)(name: c.Expr[String],
      loader: c.Expr[ClassLoader], paramTypes: c.Expr[Seq[Class[_]]])(
      args: c.Expr[Seq[AnyRef]]): c.Expr[AnyRef] = c.universe.reify {

    val clazz = loader.splice.loadClass(name.splice)
    val ctor = clazz.getConstructor(paramTypes.splice.toArray[Class[_]]: _*)
    ctor.newInstance(args.splice: _*).asInstanceOf[AnyRef]
  }

  def loadModule(name: String, loader: ClassLoader): AnyRef =
    macro loadModule_impl

  def loadModule_impl(c: Context)(name: c.Expr[String],
      loader: c.Expr[ClassLoader]): c.Expr[AnyRef] = c.universe.reify {
    val clazz = loader.splice.loadClass(name.splice + "$")
    clazz.getField("MODULE$").get(null)
  }

}
