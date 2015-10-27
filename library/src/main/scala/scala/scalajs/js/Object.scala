/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

/** Base class of all JavaScript objects. */
@native
class Object extends Any {
  def this(value: Any) = this()

  def toLocaleString(): String = native
  def valueOf(): scala.Any = native

  /** Tests whether this object has the specified property as a direct property.
   *
   *  Unlike [[js.Object.hasProperty]], this method does not check down the
   *  object's prototype chain.
   *
   * MDN
   */
  def hasOwnProperty(v: String): Boolean = native

  /** Tests whether this object is in the prototype chain of another object. */
  def isPrototypeOf(v: Object): Boolean = native

  /** Tests whether the specified property in an object can be enumerated by a
   *  call to [[js.Object.properties]], with the exception of properties
   *  inherited through the prototype chain. If the object does not have the
   *  specified property, this method returns false.
   *
   *  MDN
   */
  def propertyIsEnumerable(v: String): Boolean = native
}

/** The top-level `Object` JavaScript object. */
@native
object Object extends Object {
  def apply(): Object = native
  def apply(value: Any): Object = native

  /** Tests whether the object has a property on itself or in its prototype
   *  chain. This method is the equivalent of `p in o` in JavaScript.
   */
  def hasProperty(o: Object, p: String): Boolean = sys.error("stub")

  /**
   * The Object.getPrototypeOf() method returns the prototype (i.e. the
   * internal `Prototype`) of the specified object.
   *
   * MDN
   */
  def getPrototypeOf(o: Object): Object = native

  /**
   * The Object.getOwnPropertyDescriptor() method returns a property descriptor
   * for an own property (that is, one directly present on an object, not
   * present by dint of being along an object's prototype chain) of a given object.
   *
   * MDN
   */
  def getOwnPropertyDescriptor(o: Object, p: String): PropertyDescriptor = native

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
  def getOwnPropertyNames(o: Object): Array[String] = native

  /**
   * The Object.create() method creates a new object with the specified
   * prototype object and properties.
   *
   * MDN
   */
  def create(o: Object, properties: Any): Object = native
  def create(o: Object): Object = native

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
  def defineProperty(o: Object, p: String, attributes: PropertyDescriptor): o.type = native

  /**
   * The Object.defineProperties() method defines new or modifies existing
   * properties directly on an object, returning the object.
   *
   * MDN
   */
  def defineProperties(o: Object, properties: Any): o.type = native

  /**
   * The Object.seal() method seals an object, preventing new properties from
   * being added to it and marking all existing properties as non-configurable.
   * Values of present properties can still be changed as long as they are
   * writable.
   *
   * MDN
   */
  def seal(o: Object): o.type = native

  /**
   * The Object.freeze() method freezes an object: that is, prevents new properties
   * from being added to it; prevents existing properties from being removed;
   * and prevents existing properties, or their enumerability, configurability,
   * or writability, from being changed. In essence the object is made effectively
   * immutable. The method returns the object being frozen.
   *
   * MDN
   */
  def freeze(o: Object): o.type = native

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
  def preventExtensions(o: Object): o.type = native

  /**
   * Returns true if the object is sealed, otherwise false. An object is sealed
   * if it is not extensible and if all its properties are non-configurable and
   * therefore not removable (but not necessarily non-writable).
   *
   * MDN
   */
  def isSealed(o: Object): Boolean = native

  /**
   * The Object.isFrozen() determines if an object is frozen.
   *
   * An object is frozen if and only if it is not extensible, all its properties
   * are non-configurable, and all its data properties (that is, properties which
   * are not accessor properties with getter or setter components) are non-writable.
   *
   * MDN
   */
  def isFrozen(o: Object): Boolean = native

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
  def isExtensible(o: Object): Boolean = native

  /**
   * The Object.keys() method returns an array of a given object's own enumerable
   * properties, in the same order as that provided by a for...in loop (the
   * difference being that a for-in loop enumerates properties in the prototype
   * chain as well).
   *
   * MDN
   */
  def keys(o: Object): Array[String] = native

  /** Returns the names of all the enumerable properties of this object,
   *  including properties in its prototype chain.
   *
   *  This method returns the same set of names that would be enumerated by
   *  a for-in loop in JavaScript, but not necessarily in the same order.
   *
   *  If the underlying implementation guarantees an order for for-in loops,
   *  then this is guaranteed to be consistent with [[keys]], in the sense
   *  that the list returned by [[keys]] is a sublist of the list returned by
   *  this method (not just a subset).
   */
  def properties(o: Any): Array[String] = sys.error("stub")
}
