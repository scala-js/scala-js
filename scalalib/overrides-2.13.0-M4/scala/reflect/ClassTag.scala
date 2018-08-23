package scala
package reflect

import java.lang.{ Class => jClass }

/**
 *
 * A `ClassTag[T]` stores the erased class of a given type `T`, accessible via the `runtimeClass`
 * field. This is particularly useful for instantiating `Array`s whose element types are unknown
 * at compile time.
 *
 * `ClassTag`s are a weaker special case of [[scala.reflect.api.TypeTags#TypeTag]]s, in that they
 * wrap only the runtime class of a given type, whereas a `TypeTag` contains all static type
 * information. That is, `ClassTag`s are constructed from knowing only the top-level class of a
 * type, without necessarily knowing all of its argument types. This runtime information is enough
 * for runtime `Array` creation.
 *
 * For example:
 * {{{
 *   scala> def mkArray[T : ClassTag](elems: T*) = Array[T](elems: _*)
 *   mkArray: [T](elems: T*)(implicit evidence$1: scala.reflect.ClassTag[T])Array[T]
 *
 *   scala> mkArray(42, 13)
 *   res0: Array[Int] = Array(42, 13)
 *
 *   scala> mkArray("Japan","Brazil","Germany")
 *   res1: Array[String] = Array(Japan, Brazil, Germany)
 * }}}
 *
 * See [[scala.reflect.api.TypeTags]] for more examples, or the
 * [[http://docs.scala-lang.org/overviews/reflection/typetags-manifests.html Reflection Guide: TypeTags]]
 * for more details.
 *
 */
@scala.annotation.implicitNotFound(msg = "No ClassTag available for ${T}")
trait ClassTag[T] extends ClassManifestDeprecatedApis[T] with Equals with Serializable {
  // please, don't add any APIs here, like it was with `newWrappedArray` and `newArrayBuilder`
  // class tags, and all tags in general, should be as minimalistic as possible

  /** A class representing the type `U` to which `T` would be erased.
   *  Note that there is no subtyping relationship between `T` and `U`.
   */
  def runtimeClass: jClass[_]

  /** Produces a `ClassTag` that knows how to instantiate an `Array[Array[T]]` */
  def wrap: ClassTag[Array[T]] = ClassTag[Array[T]](arrayClass(runtimeClass))

  /** Produces a new array with element type `T` and length `len` */
  override def newArray(len: Int): Array[T] =
    runtimeClass match {
      case java.lang.Byte.TYPE      => new Array[Byte](len).asInstanceOf[Array[T]]
      case java.lang.Short.TYPE     => new Array[Short](len).asInstanceOf[Array[T]]
      case java.lang.Character.TYPE => new Array[Char](len).asInstanceOf[Array[T]]
      case java.lang.Integer.TYPE   => new Array[Int](len).asInstanceOf[Array[T]]
      case java.lang.Long.TYPE      => new Array[Long](len).asInstanceOf[Array[T]]
      case java.lang.Float.TYPE     => new Array[Float](len).asInstanceOf[Array[T]]
      case java.lang.Double.TYPE    => new Array[Double](len).asInstanceOf[Array[T]]
      case java.lang.Boolean.TYPE   => new Array[Boolean](len).asInstanceOf[Array[T]]
      case java.lang.Void.TYPE      => new Array[Unit](len).asInstanceOf[Array[T]]
      case _                        => java.lang.reflect.Array.newInstance(runtimeClass, len).asInstanceOf[Array[T]]
    }

  /** A ClassTag[T] can serve as an extractor that matches only objects of type T.
   *
   * The compiler tries to turn unchecked type tests in pattern matches into checked ones
   * by wrapping a `(_: T)` type pattern as `ct(_: T)`, where `ct` is the `ClassTag[T]` instance.
   * Type tests necessary before calling other extractors are treated similarly.
   * `SomeExtractor(...)` is turned into `ct(SomeExtractor(...))` if `T` in `SomeExtractor.unapply(x: T)`
   * is uncheckable, but we have an instance of `ClassTag[T]`.
   */
  def unapply(x: Any): Option[T] =
    if (null != x && (
            (runtimeClass.isInstance(x))
         || (x.isInstanceOf[Byte]    && runtimeClass.isAssignableFrom(classOf[Byte]))
         || (x.isInstanceOf[Short]   && runtimeClass.isAssignableFrom(classOf[Short]))
         || (x.isInstanceOf[Char]    && runtimeClass.isAssignableFrom(classOf[Char]))
         || (x.isInstanceOf[Int]     && runtimeClass.isAssignableFrom(classOf[Int]))
         || (x.isInstanceOf[Long]    && runtimeClass.isAssignableFrom(classOf[Long]))
         || (x.isInstanceOf[Float]   && runtimeClass.isAssignableFrom(classOf[Float]))
         || (x.isInstanceOf[Double]  && runtimeClass.isAssignableFrom(classOf[Double]))
         || (x.isInstanceOf[Boolean] && runtimeClass.isAssignableFrom(classOf[Boolean]))
         || (x.isInstanceOf[Unit]    && runtimeClass.isAssignableFrom(classOf[Unit])))
       ) Some(x.asInstanceOf[T])
    else None

  // TODO: deprecate overloads in 2.12.0, remove in 2.13.0
  def unapply(x: Byte)    : Option[T] = unapplyImpl(x, classOf[Byte])
  def unapply(x: Short)   : Option[T] = unapplyImpl(x, classOf[Short])
  def unapply(x: Char)    : Option[T] = unapplyImpl(x, classOf[Char])
  def unapply(x: Int)     : Option[T] = unapplyImpl(x, classOf[Int])
  def unapply(x: Long)    : Option[T] = unapplyImpl(x, classOf[Long])
  def unapply(x: Float)   : Option[T] = unapplyImpl(x, classOf[Float])
  def unapply(x: Double)  : Option[T] = unapplyImpl(x, classOf[Double])
  def unapply(x: Boolean) : Option[T] = unapplyImpl(x, classOf[Boolean])
  def unapply(x: Unit)    : Option[T] = unapplyImpl(x, classOf[Unit])

  private[this] def unapplyImpl(x: Any, primitiveCls: java.lang.Class[_]): Option[T] =
    if (runtimeClass.isInstance(x) || runtimeClass.isAssignableFrom(primitiveCls)) Some(x.asInstanceOf[T])
    else None

  // case class accessories
  override def canEqual(x: Any) = x.isInstanceOf[ClassTag[_]]
  override def equals(x: Any) = x.isInstanceOf[ClassTag[_]] && this.runtimeClass == x.asInstanceOf[ClassTag[_]].runtimeClass
  override def hashCode = runtimeClass.##
  override def toString = {
    def prettyprint(clazz: jClass[_]): String =
      if (clazz.isArray) s"Array[${prettyprint(clazz.getComponentType)}]" else
      clazz.getName
    prettyprint(runtimeClass)
  }
}

/**
 * Class tags corresponding to primitive types and constructor/extractor for ClassTags.
 */
object ClassTag {
  def Byte    : ClassTag[scala.Byte]       = ManifestFactory.Byte
  def Short   : ClassTag[scala.Short]      = ManifestFactory.Short
  def Char    : ClassTag[scala.Char]       = ManifestFactory.Char
  def Int     : ClassTag[scala.Int]        = ManifestFactory.Int
  def Long    : ClassTag[scala.Long]       = ManifestFactory.Long
  def Float   : ClassTag[scala.Float]      = ManifestFactory.Float
  def Double  : ClassTag[scala.Double]     = ManifestFactory.Double
  def Boolean : ClassTag[scala.Boolean]    = ManifestFactory.Boolean
  def Unit    : ClassTag[scala.Unit]       = ManifestFactory.Unit
  def Any     : ClassTag[scala.Any]        = ManifestFactory.Any
  def Object  : ClassTag[java.lang.Object] = ManifestFactory.Object
  def AnyVal  : ClassTag[scala.AnyVal]     = ManifestFactory.AnyVal
  def AnyRef  : ClassTag[scala.AnyRef]     = ManifestFactory.AnyRef
  def Nothing : ClassTag[scala.Nothing]    = ManifestFactory.Nothing
  def Null    : ClassTag[scala.Null]       = ManifestFactory.Null

  @inline
  private class GenericClassTag[T](val runtimeClass: jClass[_]) extends ClassTag[T]

  def apply[T](runtimeClass1: jClass[_]): ClassTag[T] =
    runtimeClass1 match {
      case java.lang.Byte.TYPE      => ClassTag.Byte.asInstanceOf[ClassTag[T]]
      case java.lang.Short.TYPE     => ClassTag.Short.asInstanceOf[ClassTag[T]]
      case java.lang.Character.TYPE => ClassTag.Char.asInstanceOf[ClassTag[T]]
      case java.lang.Integer.TYPE   => ClassTag.Int.asInstanceOf[ClassTag[T]]
      case java.lang.Long.TYPE      => ClassTag.Long.asInstanceOf[ClassTag[T]]
      case java.lang.Float.TYPE     => ClassTag.Float.asInstanceOf[ClassTag[T]]
      case java.lang.Double.TYPE    => ClassTag.Double.asInstanceOf[ClassTag[T]]
      case java.lang.Boolean.TYPE   => ClassTag.Boolean.asInstanceOf[ClassTag[T]]
      case java.lang.Void.TYPE      => ClassTag.Unit.asInstanceOf[ClassTag[T]]
      case _                        =>
        if (classOf[java.lang.Object] == runtimeClass1)
          ClassTag.Object.asInstanceOf[ClassTag[T]]
        else if (classOf[scala.runtime.Nothing$] == runtimeClass1)
          ClassTag.Nothing.asInstanceOf[ClassTag[T]]
        else if (classOf[scala.runtime.Null$] == runtimeClass1)
          ClassTag.Null.asInstanceOf[ClassTag[T]]
        else
          new GenericClassTag[T](runtimeClass1)
    }

  def unapply[T](ctag: ClassTag[T]): Option[Class[_]] = Some(ctag.runtimeClass)
}
