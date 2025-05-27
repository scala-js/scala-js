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

package org.scalajs.javalibintf

import java.util.Map.Entry
import java.util.Optional
import java.util.function.{Function, Supplier}

object Reflect {
  private val loadableModuleClasses =
    new java.util.HashMap[String, LoadableModuleClass[_]]()

  private val instantiatableClasses =
    new java.util.HashMap[String, InstantiatableClass[_]]()

  // Public API (documented in javalibintf/.../Reflect.java)

  def registerLoadableModuleClass[T](fqcn: String, runtimeClass: Class[T],
      moduleSupplier: Supplier[T]): Unit = {
    loadableModuleClasses.put(fqcn,
        new LoadableModuleClassImpl(runtimeClass, moduleSupplier))
  }

  def registerInstantiatableClass[T](fqcn: String, runtimeClass: Class[T],
      constructors: Array[Entry[Array[Class[_]], Function[Array[Object], T]]]): Unit = {
    val ctorLen = constructors.length
    val invokableConstructors = new Array[InvokableConstructor[T]](ctorLen)
    var i = 0
    while (i != ctorLen) {
      val entry = constructors(i)
      invokableConstructors(i) =
        new InvokableConstructorImpl(entry.getKey().clone(), entry.getValue())
      i += 1
    }
    instantiatableClasses.put(fqcn,
        new InstantiatableClassImpl(runtimeClass, invokableConstructors))
  }

  def lookupLoadableModuleClass(fqcn: String): Optional[LoadableModuleClass[_]] =
    Optional.ofNullable(loadableModuleClasses.get(fqcn))

  def lookupInstantiatableClass(fqcn: String): Optional[InstantiatableClass[_]] =
    Optional.ofNullable(instantiatableClasses.get(fqcn))

  trait LoadableModuleClass[T] {
    def getRuntimeClass(): Class[T]

    def loadModule(): T
  }

  trait InstantiatableClass[T] {
    def getRuntimeClass(): Class[T]

    def getDeclaredConstructors(): Array[InvokableConstructor[T]]
  }

  trait InvokableConstructor[T] {
    def getParameterTypes(): Array[Class[_]]

    def newInstance(args: Array[Object]): T
  }

  // Private implementation

  private final class LoadableModuleClassImpl[T](
      runtimeClass: Class[T],
      moduleSupplier: Supplier[T]
  ) extends LoadableModuleClass[T] {
    def getRuntimeClass(): Class[T] = runtimeClass

    def loadModule(): T = moduleSupplier.get()
  }

  private final class InstantiatableClassImpl[T](
      runtimeClass: Class[T],
      declaredConstructors: Array[InvokableConstructor[T]]
  ) extends InstantiatableClass[T] {
    def getRuntimeClass(): Class[T] = runtimeClass

    def getDeclaredConstructors(): Array[InvokableConstructor[T]] =
      declaredConstructors.clone()
  }

  private final class InvokableConstructorImpl[T](
      parameterTypes: Array[Class[_]],
      newInstanceFun: Function[Array[Object], T]
  ) extends InvokableConstructor[T] {
    def getParameterTypes(): Array[Class[_]] = parameterTypes.clone()

    def newInstance(args: Array[Object]): T = {
      /* Check the number of actual arguments. We let the casts and unbox
       * operations inside `newInstanceFun` take care of the rest.
       */
      if (args.length != parameterTypes.length)
        throw new IllegalArgumentException(s"Argument count mismatch: ${args.length}")
      newInstanceFun.apply(args)
    }
  }
}
