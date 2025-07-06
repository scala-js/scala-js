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

package org.scalajs.javalibintf;

import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Scala.js-specific reflection API.
 *
 * This class offers static methods and static inner interfaces for the
 * reflection capabilities of Scala.js.
 */
public class Reflect {
  private Reflect() {}

  /**
   * Registers a module class whose singleton instance can be loaded.
   *
   * This method is not intended to be called from user code. Compilers insert
   * calls to this method to automatically register loadable module classes,
   * according to the source language's rules.
   *
   * The Scala.js compiler calls this method for static objects that are
   * annotated with
   * <code>@scala.scalajs.reflect.annotation.EnableReflectiveInstantiation</code>,
   * or that have a super type (class or trait) with that annotation.
   *
   * @param <T>
   *   Type of the instances loaded from this module class.
   * @param fqcn
   *   Fully qualified name of the module class, including the trailing <code>$</code>.
   * @param runtimeClass
   *   {@link java.lang.Class} instance representing the module class.
   * @param moduleSupplier
   *   A supplier that returns the singleton instance of the module class.
   */
  public static <T> void registerLoadableModuleClass(
      String fqcn, Class<T> runtimeClass, Supplier<T> moduleSupplier) {
    throw new AssertionError("stub");
  }

  /**
   * Registers a class for reflective instantiation.
   *
   * This method is not intended to be called from user code. Compilers insert
   * calls to this method to automatically register instantiatable classes,
   * according to the source language's rules.
   *
   * The Scala.js compiler calls this method for classes that satisfy the
   * following requirements:
   *
   * <ul>
   *   <li>the class or one of its super types (class or trait) is annotated
   *     with <code>@scala.scalajs.reflect.annotation.EnableReflectiveInstantiation</code>,</li>
   *   <li>it is not abstract, and</li>
   *   <li>it is not a local class (i.e., a class defined inside a <code>def</code>).</li>
   * </ul>
   *
   * Inner classes (defined inside another class) are registered.
   *
   * @param <T>
   *   Type of the instantiatable class to register.
   * @param fqcn
   *   Fully qualified name of the class.
   * @param runtimeClass
   *   {@link java.lang.Class} instance representing the class.
   * @param constructors
   *   An array describing the available constructors. Each entry represents
   *   one public constructor. The key of the entry is an array of the
   *   parameter types. The value is a function that, when called with an array
   *   of arguments, instantiates the class by passing the arguments to the
   *   corresponding constructor.
   */
  public static <T> void registerInstantiatableClass(
      String fqcn, Class<T> runtimeClass,
      Entry<Class<?>[], Function<Object[], T>>[] constructors) {
    throw new AssertionError("stub");
  }

  /**
   * Reflectively looks up a loadable module class.
   *
   * A module class is the technical term referring to the class of a Scala
   * <code>object</code>.
   *
   * The module class must have been registered with
   * {@link registerLoadableModuleClass}. This is usually automatically done
   * by compilers according to the rules of the source language.
   *
   * If the module class cannot be found, this method returns an empty
   * {@link java.util.Optional}.
   *
   * @param fqcn
   *   Fully-qualified name of the module class, including its trailing <code>$</code>
   */
  public static Optional<LoadableModuleClass<?>> lookupLoadableModuleClass(String fqcn) {
    throw new AssertionError("stub");
  }

  /**
   * Reflectively looks up an instantiable class.
   *
   * The module class must have been registered with
   * {@link registerInstantiatableClass}. This is usually automatically done
   * by compilers according to the rules of the source language.
   *
   * If the class cannot be found, either because it does not exist,
   * was not <code>@EnableReflectiveInstantiation</code> or was abstract or
   * local, this method returns an empty {@link java.util.Optional}.
   *
   * @param fqcn
   *   Fully-qualified name of the class
   */
  public static Optional<InstantiatableClass<?>> lookupInstantiatableClass(String fqcn) {
    throw new AssertionError("stub");
  }

  /**
   * A handle to a loadable module class.
   *
   * @param <T>
   *   Type of the represented module class.
   */
  public static interface LoadableModuleClass<T> {
    /** The {@link java.lang.Class} instance representing the module class. */
    public Class<T> getRuntimeClass();

    /** Loads the singleton instance of the module class. */
    public T loadModule();
  }

  /**
   * A handle to an instantiatable class.
   *
   * @param <T>
   *   Type of the represented class.
   */
  public static interface InstantiatableClass<T> {
    /** The {@link java.lang.Class} instance representing the slass. */
    public Class<T> getRuntimeClass();

    /**
     * Array of all the constructors that can be reflectively invoked.
     *
     * The result is a fresh array.
     */
    public InvokableConstructor<T>[] getDeclaredConstructors();
  }

  /**
   * A handle to an invokable constructor of an instantiatable class.
   *
   * @param <T>
   *   Type of instances created by this constructor.
   */
  public static interface InvokableConstructor<T> {
    /**
     * Array of the parameter types required by this constructor.
     *
     * The result is a fresh array.
     */
    public Class<?>[] getParameterTypes();

    /**
     * Calls the constructor to create a new instance of the class.
     *
     * @param args
     *   An array of the arguments to pass to the constructor.
     * @return
     *   The newly created instance.
     */
    public T newInstance(Object... args);
  }
}
