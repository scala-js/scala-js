package org.scalajs.testinterface

import scala.scalajs.js
import scala.scalajs.reflect._

@deprecated(
    "Use scala.scalajs.reflect.Reflect (JS-only) or " +
    "https://github.com/portable-scala/portable-scala-reflect (portable) " +
    "instead.",
    "0.6.25")
object TestUtils {

  /** Instantiates the class given by its fully qualified name.
   *
   *  The target class must extend a class or trait annotated with
   *  [[scala.scalajs.reflect.annotation.EnableReflectiveInstantiation]].
   *
   *  The `paramTypes` argument is used to select the appropriate overloaded
   *  constructor.
   */
  def newInstance(name: String, loader: ClassLoader, paramTypes: Seq[Class[_]])(
      args: Seq[Any]): AnyRef = {
    require(args.size == paramTypes.size, "argument count mismatch")

    val clazz = Reflect.lookupInstantiatableClass(name).getOrElse {
      throw new InstantiationError(name)
    }
    val ctor = clazz.declaredConstructors.find {
      _.parameterTypes == paramTypes
    }.getOrElse {
      throw new InstantiationError(name)
    }
    ctor.newInstance(args: _*).asInstanceOf[AnyRef]
  }

  /** Loads the module given by its fully qualified name.
   *
   *  The target object must extend a class or trait annotated with
   *  [[scala.scalajs.reflect.annotation.EnableReflectiveInstantiation]].
   *
   *  The name *must not* include the trailing `$` that is part of the module
   *  name, as added by the Scala compiler.
   */
  def loadModule(name: String, loader: ClassLoader): AnyRef = {
    val loadableModule = Reflect.lookupLoadableModuleClass(name + "$").getOrElse {
      throw new InstantiationError(name)
    }
    loadableModule.loadModule().asInstanceOf[AnyRef]
  }

}
