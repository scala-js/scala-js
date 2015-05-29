package sbt.testing

/** A way to identify test classes and/or modules that should be discovered
 *  when the client performs discovery.
 *
 *  Scala.js: Implementations may not rely on the identity of Fingerprints,
 *  since they are serialized between JS / JVM.
 */
trait Fingerprint

/** Indicates that classes or modules with a specific annotation, either on at
 *  least one top level method or on the class or module itself, should be
 *  discovered as test classes.
 */
trait AnnotatedFingerprint extends Fingerprint {
  /** Indicates whether modules with the annotation should be considered during
   *  discovery, or just classes.
   *
   *  If a test framework allows both classes and modules, they should return
   *  two different fingerprints from <code>Framework.fingerprints</code>, one
   *  that returns <code>false</code> for <code>isModule</code> and another
   *  that returns <code>true</code>.
   */
  def isModule(): Boolean

  /** The fully qualified name of the annotation that identifies classes or
   *  modules as test classes or modules to be discovered.
   */
  def annotationName(): String
}

/** Indicates that classes (and possibly modules) that extend a particular
 *  superclass, or mix in a particular supertrait, should be discovered as test
 *  classes.
 */
trait SubclassFingerprint extends Fingerprint {

  /** Indicates whether modules (singleton objects) that extend the superclass
   *  or supertrait should be considered during discovery, or just classes.
   *
   *  If modules are not allowed by the test framework, they should return
   *  <code>false</code> for <code>isModule</code>. Returning
   *  <code>false</code> will speed up discovery because classes for modules
   *  can be quickly bypassed.
   */
  def isModule(): Boolean

  /** The name of the superclass or supertrait that identifies classes (and
   *  possibly modules) as test classes to be discovered.
   */
  def superclassName(): String

  /** Indicates whether discovered classes must have a no-arg constructor.
   *
   *  If this method returns <code>true</code>, the client should not discover
   *  any subclass of the given <code>superClassName</code> that does not
   *  declare a no-arg constructor, <em>i.e.</em>, a constructor that takes no
   *  arguments.
   */
  def requireNoArgConstructor(): Boolean
}
