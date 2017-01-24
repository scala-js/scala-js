package org.scalajs.testinterface

import scala.scalajs.js
import scala.scalajs.reflect._

object TestUtils {

  /** Instantiates the class given by its fully qualified name.
   *
   *  The target class must be exported under its fully qualified name.
   *
   *  This overload of `newInstance` cannot instantiate classes with an
   *  ancestor annotated with
   *  [[scala.scalajs.reflect.annotation.EnableReflectiveInstantiation]].
   *
   *  Prefer using the other overload of `newInstance` for new code, which
   *  supports reflective instantiation in addition to exports-based
   *  instantiation.
   */
  @deprecated(
      "Use the overload with explicit formal constructor parameter types.",
      "0.6.15")
  def newInstance(name: String, loader: ClassLoader)(
      args: Seq[AnyRef]): AnyRef = {
    Reflect.lookupInstantiatableClass(name).fold[AnyRef] {
      val ctor = deepSelect(namespace(loader), name)
      js.Dynamic.newInstance(ctor)(args.asInstanceOf[Seq[js.Any]]: _*)
    } { clazz =>
      throw new InstantiationException(
          s"The class '$name' should be loaded through reflective " +
          "instantiation, but the overload of TestUtils.newIntance() that " +
          "was used does not support it. You can fix it by calling the other " +
          "overload of TestUtils.newInstance().")
    }
  }

  /** Instantiates the class given by its fully qualified name.
   *
   *  The target class must either
   *
   *  - extend a class or trait annotated with
   *    [[scala.scalajs.reflect.annotation.EnableReflectiveInstantiation]], or
   *  - be exported under its fully qualified name.
   *
   *  In the former case, the overload is selected based on `paramTypes`. In
   *  the latter case, the overload is selected by the usual export overload
   *  resolution mechanism.
   */
  def newInstance(name: String, loader: ClassLoader, paramTypes: Seq[Class[_]])(
      args: Seq[Any]): AnyRef = {
    require(args.size == paramTypes.size, "argument count mismatch")

    Reflect.lookupInstantiatableClass(name).fold[AnyRef] {
      val ctor = deepSelect(namespace(loader), name)
      js.Dynamic.newInstance(ctor)(args.asInstanceOf[Seq[js.Any]]: _*)
    } { clazz =>
      val ctor = clazz.declaredConstructors.find {
        _.parameterTypes == paramTypes
      }.getOrElse {
        throw new InstantiationError(name)
      }
      ctor.newInstance(args: _*).asInstanceOf[AnyRef]
    }
  }

  /** Loads the module given by its fully qualified name.
   *
   *  The target object must either
   *
   *  - extend a class or trait annotated with
   *    [[scala.scalajs.reflect.annotation.EnableReflectiveInstantiation]], or
   *  - be exported under its fully qualified name.
   *
   *  The name *must not* include the trailing `$` that is part of the module
   *  name, as added by the Scala compiler.
   */
  def loadModule(name: String, loader: ClassLoader): AnyRef = {
    Reflect.lookupLoadableModuleClass(name + "$").fold[AnyRef] {
      val accessor = deepSelect(namespace(loader), name)
      accessor()
    } { loadableModule =>
      loadableModule.loadModule().asInstanceOf[AnyRef]
    }
  }

  private def namespace(loader: ClassLoader): js.Dynamic = {
    loader match {
      case loader: ScalaJSClassLoader =>
        loader.namespace
      case _ =>
        throw new IllegalArgumentException("Need a ScalaJSClassLoader.")
    }
  }

  private def deepSelect(receiver: js.Dynamic, name: String) =
    name.split('.').foldLeft(receiver)((obj, n) => obj.selectDynamic(n))

}
