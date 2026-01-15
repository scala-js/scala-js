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

/** All doc-comments marked as "MDN" are by Mozilla Contributors,
 *  distributed under the Creative Commons Attribution-ShareAlike license from
 *  https://developer.mozilla.org/en-US/docs/Web/Reference/API
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
   *  MDN
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

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  The `Object.assign()` method is used to copy the values of all enumerable
   *  own properties from one or more source objects to a target object. It will
   *  return the target object.
   *
   *  Properties in the target object will be overwritten by properties in the
   *  sources if they have the same key. Later sources' properties will
   *  similarly overwrite earlier ones.
   *
   *  The `Object.assign()` method only copies enumerable and own properties from
   *  a source object to a target object. It uses `Get` on the source and `Set`
   *  on the target, so it will invoke getters and setters. Therefore it
   *  assigns properties versus just copying or defining new properties. This may
   *  make it unsuitable for merging new properties into a prototype if the merge
   *  sources contain getters. For copying property definitions, including their
   *  enumerability, into prototypes `Object.getOwnPropertyDescriptor()` and
   *  `Object.defineProperty()` should be used instead.
   *
   *  Both `String` and `Symbol` properties are copied.
   *
   *  In case of an error, for example if a property is non-writable, a
   *  `TypeError` will be raised, and the target object can be changed if any
   *  properties are added before error is raised.
   *
   *  Note that `Object.assign()` does not throw on null or undefined source
   *  values.
   *
   *  MDN
   */
  def assign(t: js.Object, s: js.Object*): js.Object = js.native

  /** The Object.getPrototypeOf() method returns the prototype (i.e. the
   *  internal `Prototype`) of the specified object.
   *
   *  MDN
   */
  def getPrototypeOf(o: js.Object): js.Object = js.native

  /** The Object.getOwnPropertyDescriptor() method returns a property descriptor
   *  for an own property (that is, one directly present on an object, not
   *  present by dint of being along an object's prototype chain) of a given object.
   *
   *  MDN
   */
  def getOwnPropertyDescriptor(o: js.Object,
      p: String): js.PropertyDescriptor = js.native

  /** Object.getOwnPropertyNames returns an array whose elements are strings
   *  corresponding to the enumerable and non-enumerable properties found
   *  directly upon obj. The ordering of the enumerable properties in the array
   *  is consistent with the ordering exposed by a for...in loop (or by Object.keys)
   *  over the properties of the object. The ordering of the non-enumerable
   *  properties in the array, and among the enumerable properties, is not defined.
   *
   *  MDN
   */
  def getOwnPropertyNames(o: js.Object): js.Array[String] = js.native

  /** <span class="badge badge-ecma2015" style="float: right;">ECMAScript 2015</span>
   *
   *  The Object.getOwnPropertySymbols() method returns an array of all symbol
   *  properties found directly upon a given object.
   *
   *  MDN
   */
  def getOwnPropertySymbols(o: js.Object): js.Array[js.Symbol] = js.native

  /** The Object.create() method creates a new object with the specified
   *  prototype object and properties.
   *
   *  MDN
   */
  def create(o: js.Object, properties: js.Any): js.Object = js.native
  def create(o: js.Object): js.Object = native

  /** The Object.defineProperty() method defines a new property directly on an
   *  object, or modifies an existing property on an object, and returns the
   *  object.
   *
   *  This method allows precise addition to or modification of a property on an
   *  object. Normal property addition through assignment creates properties
   *  which show up during property enumeration (for...in loop or Object.keys method),
   *  whose values may be changed, and which may be deleted. This method allows
   *  these extra details to be changed from their defaults.
   *
   *  Property descriptors present in objects come in two main flavors: data
   *  descriptors and accessor descriptors. A data descriptor is a property
   *  that has a value, which may or may not be writable. An accessor descriptor
   *  is a property described by a getter-setter pair of functions. A descriptor
   *  must be one of these two flavors; it cannot be both.
   *
   *  MDN
   */
  def defineProperty(o: js.Object, p: String,
      attributes: js.PropertyDescriptor): o.type = js.native

  /** The Object.defineProperties() method defines new or modifies existing
   *  properties directly on an object, returning the object.
   *
   *  MDN
   */
  def defineProperties(o: js.Object, properties: js.Any): o.type = js.native

  /** The Object.seal() method seals an object, preventing new properties from
   *  being added to it and marking all existing properties as non-configurable.
   *  Values of present properties can still be changed as long as they are
   *  writable.
   *
   *  MDN
   */
  def seal(o: js.Object): o.type = js.native

  /** The Object.freeze() method freezes an object: that is, prevents new properties
   *  from being added to it; prevents existing properties from being removed;
   *  and prevents existing properties, or their enumerability, configurability,
   *  or writability, from being changed. In essence the object is made effectively
   *  immutable. The method returns the object being frozen.
   *
   *  MDN
   */
  def freeze(o: js.Object): o.type = js.native

  /** The Object.preventExtensions() method prevents new properties from ever
   *  being added to an object (i.e. prevents future extensions to the object).
   *
   *  An object is extensible if new properties can be added to it.  preventExtensions
   *  marks an object as no longer extensible, so that it will never have
   *  properties beyond the ones it had at the time it was marked as non-extensible.
   *  Note that the properties of a non-extensible object, in general, may still be
   *  deleted. Attempting to add new properties to a non-extensible object will
   *  fail, either silently or by throwing a TypeError (most commonly, but not
   *  exclusively, when in strict mode).
   *
   *  Object.preventExtensions only prevents addition of own properties. Properties
   *  can still be added to the object prototype. However, calling Object.preventExtensions
   *  on an object will also prevent extensions on its __proto__ property.
   *
   *  MDN
   */
  def preventExtensions(o: js.Object): o.type = js.native

  /** <span class="badge badge-ecma2015" style="float: right;">ECMAScript 2015</span>
   *
   *  Object.is() determines whether two values are the same value. Two values
   *  are the same if one of the following holds:
   *
   *  <ul>
   *    <li>both undefined
   *    <li>both null
   *    <li>both true or both false
   *    <li>both strings of the same length with the same characters in the same
   *        order
   *    <li>both the same object (means both object have same reference)
   *    <li>both numbers and
   *      <ul>
   *        <li>both +0
   *        <li>both -0
   *        <li>both NaN
   *        <li>or both non-zero and both not NaN and both have the same value
   *      </ul>
   *    </li>
   *  </ul>
   *
   *  This is not the same as being equal according to JavaScript's `===`
   *  operator (exposed as `js.special.strictEquals`` in Scala.js). The `===`
   *  operator treats the number values `-0` and `+0` as equal and treats `NaN`
   *  as not equal to `NaN`.
   *
   *  MDN
   */
  def is(value1: scala.Any, value2: scala.Any): Boolean = js.native

  /** Returns true if the object is sealed, otherwise false. An object is sealed
   *  if it is not extensible and if all its properties are non-configurable and
   *  therefore not removable (but not necessarily non-writable).
   *
   *  MDN
   */
  def isSealed(o: js.Object): Boolean = js.native

  /** The Object.isFrozen() determines if an object is frozen.
   *
   *  An object is frozen if and only if it is not extensible, all its properties
   *  are non-configurable, and all its data properties (that is, properties which
   *  are not accessor properties with getter or setter components) are non-writable.
   *
   *  MDN
   */
  def isFrozen(o: js.Object): Boolean = js.native

  /** Determines if extending of an object is allowed
   *
   *  Objects are extensible by default: they can have new properties added to
   *  them, and (in engines that support __proto__  their __proto__ property)
   *  can be modified. An object can be marked as non-extensible using
   *  Object.preventExtensions, Object.seal, or Object.freeze
   *
   *  MDN
   */
  def isExtensible(o: js.Object): Boolean = js.native

  /** The Object.keys() method returns an array of a given object's own enumerable
   *  properties, in the same order as that provided by a for...in loop (the
   *  difference being that a for-in loop enumerates properties in the prototype
   *  chain as well).
   *
   *  MDN
   */
  def keys(o: js.Object): js.Array[String] = js.native

  /** <span class="badge badge-ecma2017" style="float: right;">ECMAScript 2017</span>
   *
   *  The Object.entries() method returns an array of a given object's own
   *  enumerable string-keyed property [key, value] pairs, in the same order as
   *  that provided by a for...in loop (the difference being that a for-in loop
   *  enumerates properties in the prototype chain as well).
   *
   *  MDN
   */
  def entries(o: js.Object): js.Array[js.Tuple2[String, scala.Any]] = js.native

  /** <span class="badge badge-ecma2017" style="float: right;">ECMAScript 2017</span> */
  def entries[A](
      dict: js.Dictionary[A]): js.Array[js.Tuple2[String, A]] = js.native

  /** <span class="badge badge-ecma2020" style="float: right;">ECMAScript 2020</span>
   *
   *  The Object.fromEntries() method transforms a list of key-value pairs into
   *  an object.
   *
   *  MDN
   */
  def fromEntries[A](
      iterable: js.Iterable[js.Tuple2[String, A]]): js.Dictionary[A] = js.native
}
