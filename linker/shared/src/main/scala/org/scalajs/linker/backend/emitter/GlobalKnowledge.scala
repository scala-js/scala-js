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

package org.scalajs.linker.backend.emitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{AnyFieldDef, MethodDef, JSNativeLoadSpec}
import org.scalajs.ir.Types.Type

import org.scalajs.linker.standard.ModuleSet.ModuleID

private[emitter] trait GlobalKnowledge {
  /** Tests whether the `java.lang.Class` class is instantiated. */
  def isClassClassInstantiated: Boolean

  /** Tests whether the parent class data is accessed in the linking unit. */
  def isParentDataAccessed: Boolean

  /** Tests whether the specified class name refers to an `Interface`. */
  def isInterface(className: ClassName): Boolean

  /** All the `FieldDef`s, included inherited ones, of a Scala class.
   *
   *  It is invalid to call this method with anything but a `Class` or
   *  `ModuleClass`.
   */
  def getAllScalaClassFieldDefs(className: ClassName): List[AnyFieldDef]

  /** Tests whether the specified class uses an inlineable init.
   *
   *  When it does, its (only) `init___` method is inlined inside the JS
   *  constructor. This means that instantiation should look like
   *  {{{
   *  new \$c_Foo(args)
   *  }}}
   *  instead of
   *  {{{
   *  new \$c_Foo().init___XY(args)
   *  }}}
   */
  def hasInlineableInit(className: ClassName): Boolean

  /** Tests whether the specified class locally stores its super class. */
  def hasStoredSuperClass(className: ClassName): Boolean

  /** Tests whether the specified class has any instances at all. */
  def hasInstances(className: ClassName): Boolean

  /** Gets the types of the `jsClassCaptures` of the given class. */
  def getJSClassCaptureTypes(className: ClassName): Option[List[Type]]

  /** `None` for non-native JS classes/objects; `Some(spec)` for native JS
   *  classes/objects.
   *
   *  It is invalid to call this method with a class that is not a JS class
   *  or object (native or not), or one that has JS class captures.
   */
  def getJSNativeLoadSpec(className: ClassName): Option[JSNativeLoadSpec]

  /** The JS native load spec of a native JS member. */
  def getJSNativeLoadSpec(className: ClassName,
      member: MethodName): JSNativeLoadSpec

  /** The `className` of the superclass of a (non-native) JS class.
   *
   *  It is invalid to call this method with a class that is not a non-native
   *  JS class.
   */
  def getSuperClassOfJSClass(className: ClassName): ClassName

  /** The `FieldDef`s of a class. */
  def getFieldDefs(className: ClassName): List[AnyFieldDef]

  /** The global variables that mirror a given static field. */
  def getStaticFieldMirrors(field: FieldName): List[String]

  /** The module containing this class definition.
   *
   *  It is invalid to call this method for an abstract class.
   */
  def getModule(className: ClassName): ModuleID

  /** The list of all the concrete non-static methods in representative classes.
   *
   *  The list of method names is ordered for stability.
   *
   *  Each method name is associated with a set of representative classes that
   *  have an implementation for that method. That set is not ordered.
   */
  def methodsInRepresentativeClasses(): List[(MethodName, Set[ClassName])]

  /** The public (non-static) methods of java.lang.Object. */
  def methodsInObject(): List[MethodDef]

  /** Hijacked classes that are strict descendants of `className`. */
  def hijackedDescendants(className: ClassName): Set[ClassName]

  /** Equivalent to `hijackedDescendants(className).nonEmpty` but more efficient. */
  def isAncestorOfHijackedClass(className: ClassName): Boolean
}
