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

/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Base class of all JavaScript objects. */
@js.native
@JSGlobal
class Object extends js.Any {
  def this(value: scala.Any) = this()

  def toLocaleString(): String = js.native
  def valueOf(): scala.Any = js.native

  /** Tests whether this object has the specified property as a direct property.
   *
   *  Unlike [[js.Any.ObjectCompanionOps.hasProperty js.Object.hasProperty]],
   *  this method does not check down the object's prototype chain.
   *
   * MDN
   */
  def hasOwnProperty(v: String): Boolean = js.native

  /** Tests whether this object is in the prototype chain of another object. */
  def isPrototypeOf(v: Object): Boolean = js.native

  /** Tests whether the specified property in an object can be enumerated by a
   *  call to [[js.Any.ObjectCompanionOps.properties js.Object.properties]],
   *  with the exception of properties inherited through the prototype chain.
   *
   *  If the object does not have the specified property, this method returns
   *  false.
   *
   *  MDN
   */
  def propertyIsEnumerable(v: String): Boolean = js.native
}

/** The top-level `Object` JavaScript object. */
@js.native
@JSGlobal
object Object extends js.Object {
  def apply(): js.Object = js.native
  def apply(value: scala.Any): js.Object = js.native

  /**
   * The Object.getPrototypeOf() method returns the prototype (i.e. the
   * internal `Prototype`) of the specified object.
   *
   * MDN
   */
  def getPrototypeOf(o: js.Object): js.Object = js.native

  /**
   * The Object.getOwnPropertyDescriptor() method returns a property descriptor
   * for an own property (that is, one directly present on an object, not
   * present by dint of being along an object's prototype chain) of a given object.
   *
   * MDN
   */
  def getOwnPropertyDescriptor(o: js.Object,
      p: String): js.PropertyDescriptor = js.native

  /**
   * Object.getOwnPropertyNames returns an array whose elements are strings
   * corresponding to the enumerable and non-enumerable properties found
   * directly upon obj. The ordering of the enumerable properties in the array
   * is consistent with the ordering exposed by a for...in loop (or by Object.keys)
   * over the properties of the object. The ordering of the non-enumerable
   * properties in the array, and among the enumerable properties, is not defined.
   *
   * MDN
   */
  def getOwnPropertyNames(o: js.Object): js.Array[String] = js.native

  /**
   * The Object.create() method creates a new object with the specified
   * prototype object and properties.
   *
   * MDN
   */
  def create(o: js.Object, properties: js.Any): js.Object = js.native
  def create(o: js.Object): js.Object = native

  /**
   * The Object.defineProperty() method defines a new property directly on an
   * object, or modifies an existing property on an object, and returns the
   * object.
   *
   * This method allows precise addition to or modification of a property on an
   * object. Normal property addition through assignment creates properties
   * which show up during property enumeration (for...in loop or Object.keys method),
   * whose values may be changed, and which may be deleted. This method allows
   * these extra details to be changed from their defaults.
   *
   * Property descriptors present in objects come in two main flavors: data
   * descriptors and accessor descriptors. A data descriptor is a property
   * that has a value, which may or may not be writable. An accessor descriptor
   * is a property described by a getter-setter pair of functions. A descriptor
   * must be one of these two flavors; it cannot be both.
   *
   * MDN
   */
  def defineProperty(o: js.Object, p: String,
      attributes: js.PropertyDescriptor): o.type = js.native

  /**
   * The Object.defineProperties() method defines new or modifies existing
   * properties directly on an object, returning the object.
   *
   * MDN
   */
  def defineProperties(o: js.Object, properties: js.Any): o.type = js.native

  /**
   * The Object.seal() method seals an object, preventing new properties from
   * being added to it and marking all existing properties as non-configurable.
   * Values of present properties can still be changed as long as they are
   * writable.
   *
   * MDN
   */
  def seal(o: js.Object): o.type = js.native

  /**
   * The Object.freeze() method freezes an object: that is, prevents new properties
   * from being added to it; prevents existing properties from being removed;
   * and prevents existing properties, or their enumerability, configurability,
   * or writability, from being changed. In essence the object is made effectively
   * immutable. The method returns the object being frozen.
   *
   * MDN
   */
  def freeze(o: js.Object): o.type = js.native

  /**
   * The Object.preventExtensions() method prevents new properties from ever
   * being added to an object (i.e. prevents future extensions to the object).
   *
   * An object is extensible if new properties can be added to it.  preventExtensions
   * marks an object as no longer extensible, so that it will never have
   * properties beyond the ones it had at the time it was marked as non-extensible.
   * Note that the properties of a non-extensible object, in general, may still be
   * deleted. Attempting to add new properties to a non-extensible object will
   * fail, either silently or by throwing a TypeError (most commonly, but not
   * exclusively, when in strict mode).
   *
   * Object.preventExtensions only prevents addition of own properties. Properties
   * can still be added to the object prototype. However, calling Object.preventExtensions
   * on an object will also prevent extensions on its __proto__ property.
   *
   * MDN
   */
  def preventExtensions(o: js.Object): o.type = js.native

  /**
   * Returns true if the object is sealed, otherwise false. An object is sealed
   * if it is not extensible and if all its properties are non-configurable and
   * therefore not removable (but not necessarily non-writable).
   *
   * MDN
   */
  def isSealed(o: js.Object): Boolean = js.native

  /**
   * The Object.isFrozen() determines if an object is frozen.
   *
   * An object is frozen if and only if it is not extensible, all its properties
   * are non-configurable, and all its data properties (that is, properties which
   * are not accessor properties with getter or setter components) are non-writable.
   *
   * MDN
   */
  def isFrozen(o: js.Object): Boolean = js.native

  /**
   * Determines if extending of an object is allowed
   *
   * Objects are extensible by default: they can have new properties added to
   * them, and (in engines that support __proto__  their __proto__ property)
   * can be modified. An object can be marked as non-extensible using
   * Object.preventExtensions, Object.seal, or Object.freeze
   *
   * MDN
   */
  def isExtensible(o: js.Object): Boolean = js.native

  /**
   * The Object.keys() method returns an array of a given object's own enumerable
   * properties, in the same order as that provided by a for...in loop (the
   * difference being that a for-in loop enumerates properties in the prototype
   * chain as well).
   *
   * MDN
   */
  def keys(o: js.Object): js.Array[String] = js.native
}
