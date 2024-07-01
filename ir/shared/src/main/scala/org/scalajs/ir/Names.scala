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

package org.scalajs.ir

import scala.annotation.{switch, tailrec}

import Types._

object Names {
  // scalastyle:off equals.hash.code
  // we define hashCode() once in Name, but equals() separately in its subclasses

  sealed abstract class Name(val encoded: UTF8String) {
    type ThisName <: Name

    // Eagerly compute the hash code
    private val _hashCode = UTF8String.hashCode(encoded)

    override def hashCode(): Int = _hashCode

    protected final def equalsName(that: ThisName): Boolean = {
      this._hashCode == that._hashCode && // fail fast on different hash codes
      UTF8String.equals(this.encoded, that.encoded)
    }

    def compareTo(that: ThisName): Int = {
      // scalastyle:off return
      val thisEncoded = this.encoded
      val thatEncoded = that.encoded
      val thisEncodedLen = thisEncoded.length
      val thatEncodedLen = thatEncoded.length
      val minLen = Math.min(thisEncodedLen, thatEncodedLen)
      var i = 0
      while (i != minLen) {
        val cmp = java.lang.Byte.compare(thisEncoded(i), thatEncoded(i))
        if (cmp != 0)
          return cmp
        i += 1
      }
      Integer.compare(thisEncodedLen, thatEncodedLen)
      // scalastyle:on return
    }

    protected def stringPrefix: String

    final def nameString: String =
      encoded.toString()

    override def toString(): String =
      stringPrefix + "<" + nameString + ">"
  }

  /** The name of a local variable or capture parameter.
   *
   *  Local names must be non-empty, and can contain any Unicode code point
   *  except `/ . ; [`.
   */
  final class LocalName private (encoded: UTF8String)
      extends Name(encoded) with Comparable[LocalName] {

    type ThisName = LocalName

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: LocalName => equalsName(that)
        case _               => false
      })
    }

    protected def stringPrefix: String = "LocalName"

    final def withPrefix(prefix: LocalName): LocalName =
      new LocalName(prefix.encoded ++ this.encoded)

    final def withPrefix(prefix: String): LocalName =
      LocalName(UTF8String(prefix) ++ this.encoded)

    final def withSuffix(suffix: LocalName): LocalName =
      new LocalName(this.encoded ++ suffix.encoded)

    final def withSuffix(suffix: String): LocalName =
      LocalName(this.encoded ++ UTF8String(suffix))
  }

  object LocalName {
    def apply(name: UTF8String): LocalName =
      new LocalName(validateSimpleEncodedName(name))

    def apply(name: String): LocalName =
      LocalName(UTF8String(name))

    private[Names] def fromSimpleFieldName(name: SimpleFieldName): LocalName =
      new LocalName(name.encoded)
  }

  /** The name of the label of a `Labeled` block.
   *
   *  Label names must be non-empty, and can contain any Unicode code point
   *  except `/ . ; [`.
   */
  final class LabelName private (encoded: UTF8String)
      extends Name(encoded) with Comparable[LabelName] {

    type ThisName = LabelName

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: LabelName => equalsName(that)
        case _               => false
      })
    }

    protected def stringPrefix: String = "LabelName"

    final def withSuffix(suffix: LabelName): LabelName =
      new LabelName(this.encoded ++ suffix.encoded)

    final def withSuffix(suffix: String): LabelName =
      LabelName(this.encoded ++ UTF8String(suffix))
  }

  object LabelName {
    def apply(name: UTF8String): LabelName =
      new LabelName(validateSimpleEncodedName(name))

    def apply(name: String): LabelName =
      LabelName(UTF8String(name))
  }

  /** The simple name of a field (excluding its enclosing class).
   *
   *  Field names must be non-empty, and can contain any Unicode code point
   *  except `/ . ; [`.
   */
  final class SimpleFieldName private (encoded: UTF8String)
      extends Name(encoded) with Comparable[SimpleFieldName] {

    type ThisName = SimpleFieldName

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: SimpleFieldName => equalsName(that)
        case _                     => false
      })
    }

    protected def stringPrefix: String = "SimpleFieldName"

    final def withSuffix(suffix: String): SimpleFieldName =
      SimpleFieldName(this.encoded ++ UTF8String(suffix))

    final def toLocalName: LocalName =
      LocalName.fromSimpleFieldName(this)
  }

  object SimpleFieldName {
    def apply(name: UTF8String): SimpleFieldName =
      new SimpleFieldName(validateSimpleEncodedName(name))

    def apply(name: String): SimpleFieldName =
      SimpleFieldName(UTF8String(name))
  }

  /** The full name of a field, including its simple name and its enclosing
   *  class name.
   */
  final class FieldName private (
      val className: ClassName, val simpleName: SimpleFieldName)
      extends Comparable[FieldName] {

    import FieldName._

    private val _hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      var acc = -1025990011 // "FieldName".hashCode()
      acc = mix(acc, className.##)
      acc = mix(acc, simpleName.##)
      finalizeHash(acc, 2)
    }

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: FieldName =>
          this._hashCode == that._hashCode && // fail fast on different hash codes
          this.className == that.className &&
          this.simpleName == that.simpleName
        case _ =>
          false
      })
    }

    override def hashCode(): Int = _hashCode

    def compareTo(that: FieldName): Int = {
      val classNameCmp = this.className.compareTo(that.className)
      if (classNameCmp != 0)
        classNameCmp
      else
        this.simpleName.compareTo(that.simpleName)
    }

    protected def stringPrefix: String = "FieldName"

    def nameString: String =
      className.nameString + "::" + simpleName.nameString

    override def toString(): String =
      "FieldName<" + nameString + ">"
  }

  object FieldName {
    def apply(className: ClassName, simpleName: SimpleFieldName): FieldName =
      new FieldName(className, simpleName)
  }

  /** The simple name of a method (excluding its signature).
   *
   *  Simple names must be non-empty, and can contain any Unicode code point
   *  except `/ . ; [`. In addition, they must not contain the code point `<`
   *  unless they are one of `<init>`, `<stinit>` or `<clinit>`.
   */
  final class SimpleMethodName private (encoded: UTF8String)
      extends Name(encoded) with Comparable[SimpleMethodName] {

    type ThisName = SimpleMethodName

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: SimpleMethodName => equalsName(that)
        case _                      => false
      })
    }

    protected def stringPrefix: String = "SimpleMethodName"

    /** Returns `true` iff this is the name of an instance constructor. */
    def isConstructor: Boolean =
      this eq SimpleMethodName.Constructor // globally unique, so `eq` is fine

    /** Returns `true` iff this is the name of a static initializer. */
    def isStaticInitializer: Boolean =
      this eq SimpleMethodName.StaticInitializer // globally unique, so `eq` is fine

    /** Returns `true` iff this is the name of a class initializer. */
    def isClassInitializer: Boolean =
      this eq SimpleMethodName.ClassInitializer // globally unique, so `eq` is fine
  }

  object SimpleMethodName {
    private final val ConstructorSimpleEncodedName: UTF8String =
      UTF8String("<init>")

    private final val StaticInitializerSimpleEncodedName: UTF8String =
      UTF8String("<stinit>")

    private final val ClassInitializerSimpleEncodedName: UTF8String =
      UTF8String("<clinit>")

    /** The unique `SimpleMethodName` with encoded name `<init>`. */
    val Constructor: SimpleMethodName =
      new SimpleMethodName(ConstructorSimpleEncodedName)

    /** The unique `SimpleMethodName` with encoded name `<stinit>`. */
    val StaticInitializer: SimpleMethodName =
      new SimpleMethodName(StaticInitializerSimpleEncodedName)

    /** The unique `SimpleMethodName` with encoded name `<clinit>`. */
    val ClassInitializer: SimpleMethodName =
      new SimpleMethodName(ClassInitializerSimpleEncodedName)

    def apply(name: UTF8String): SimpleMethodName = {
      val len = name.length
      if (len == 0)
        throwInvalidEncodedName(name)

      /* Handle constructor names and static initializer names. When we find
       * those, we normalize the returned instance to be one of the unique
       * instances, ensuring that they remain globally unique.
       */
      if (name(0) == '<') {
        // Must be one of '<init>', '<stinit>' or '<clinit>'
        len match {
          case 6 if UTF8String.equals(name, ConstructorSimpleEncodedName) =>
            Constructor
          case 8 if UTF8String.equals(name, StaticInitializerSimpleEncodedName) =>
            StaticInitializer
          case 8 if UTF8String.equals(name, ClassInitializerSimpleEncodedName) =>
            ClassInitializer
          case _ =>
            throwInvalidEncodedName(name)
        }
      } else {
        // Normal method name
        new SimpleMethodName(
            validateSimpleEncodedName(name, 0, len, openAngleBracketOK = false))
      }
    }

    def apply(name: String): SimpleMethodName =
      SimpleMethodName(UTF8String(name))
  }

  @deprecated("Use SimpleMethodName.Constructor instead", "1.14.0")
  def ConstructorSimpleName: SimpleMethodName =
    SimpleMethodName.Constructor

  @deprecated("Use SimpleMethodName.StaticInitializer instead", "1.14.0")
  def StaticInitializerSimpleName: SimpleMethodName =
    SimpleMethodName.StaticInitializer

  @deprecated("Use SimpleMethodName.ClassInitializer instead", "1.14.0")
  def ClassInitializerSimpleName: SimpleMethodName =
    SimpleMethodName.ClassInitializer

  /** The full name of a method, including its simple name and its signature.
   */
  final class MethodName private (val simpleName: SimpleMethodName,
      val paramTypeRefs: List[TypeRef], val resultTypeRef: TypeRef,
      val isReflectiveProxy: Boolean)
      extends Comparable[MethodName] {

    import MethodName._

    private val _hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      var acc = 1270301484 // "MethodName".hashCode()
      acc = mix(acc, simpleName.##)
      acc = mix(acc, paramTypeRefs.##)
      acc = mix(acc, resultTypeRef.##)
      acc = mixLast(acc, isReflectiveProxy.##)
      finalizeHash(acc, 4)
    }

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: MethodName =>
          this._hashCode == that._hashCode && // fail fast on different hash codes
          this.simpleName == that.simpleName &&
          this.paramTypeRefs == that.paramTypeRefs &&
          this.resultTypeRef == that.resultTypeRef &&
          this.isReflectiveProxy == that.isReflectiveProxy
        case _ =>
          false
      })
    }

    override def hashCode(): Int = _hashCode

    def compareTo(that: MethodName): Int = {
      @tailrec
      def compareParamTypeRefs(xs: List[TypeRef], ys: List[TypeRef]): Int = (xs, ys) match {
        case (x :: xr, y :: yr) =>
          val cmp = x.compareTo(y)
          if (cmp != 0) cmp
          else compareParamTypeRefs(xr, yr)
        case _ =>
          java.lang.Boolean.compare(xs.isEmpty, ys.isEmpty)
      }

      val simpleCmp = this.simpleName.compareTo(that.simpleName)
      if (simpleCmp != 0) {
        simpleCmp
      } else {
        val paramsCmp = compareParamTypeRefs(this.paramTypeRefs, that.paramTypeRefs)
        if (paramsCmp != 0) {
          paramsCmp
        } else {
          val reflProxyCmp = java.lang.Boolean.compare(
              this.isReflectiveProxy, that.isReflectiveProxy)
          if (reflProxyCmp != 0)
            reflProxyCmp
          else
            this.resultTypeRef.compareTo(that.resultTypeRef)
        }
      }
    }

    protected def stringPrefix: String = "MethodName"

    def nameString: String = {
      val builder = new java.lang.StringBuilder

      def appendTypeRef(typeRef: TypeRef): Unit = typeRef match {
        case PrimRef(tpe) =>
          tpe match {
            case NoType      => builder.append('V')
            case BooleanType => builder.append('Z')
            case CharType    => builder.append('C')
            case ByteType    => builder.append('B')
            case ShortType   => builder.append('S')
            case IntType     => builder.append('I')
            case LongType    => builder.append('J')
            case FloatType   => builder.append('F')
            case DoubleType  => builder.append('D')
            case NullType    => builder.append('N')
            case NothingType => builder.append('E')
          }
        case ClassRef(className) =>
          builder.append('L').append(className.nameString)
        case ArrayTypeRef(base, dimensions) =>
          var i = 0
          while (i != dimensions) {
            builder.append('[')
            i += 1
          }
          appendTypeRef(base)
        case ClosureTypeRef(paramTypeRefs, resultTypeRef) =>
          builder.append('(')
          var first = true
          for (paramTypeRef <- paramTypeRefs) {
            if (first)
              first = false
            else
              builder.append(',')
            appendTypeRef(paramTypeRef)
          }
          builder.append(')')
          appendTypeRef(resultTypeRef)
      }

      builder.append(simpleName.nameString)
      for (paramTypeRef <- paramTypeRefs) {
        builder.append(';')
        appendTypeRef(paramTypeRef)
      }
      builder.append(';')
      if (isReflectiveProxy)
        builder.append('R')
      else
        appendTypeRef(resultTypeRef)
      builder.toString()
    }

    override def toString(): String =
      "MethodName<" + nameString + ">"

    def displayName: String = {
      simpleName.nameString + "(" +
      paramTypeRefs.map(_.displayName).mkString(",") + ")" +
      (if (isReflectiveProxy) "R" else resultTypeRef.displayName)
    }

    /** Returns `true` iff this is the name of an instance constructor. */
    def isConstructor: Boolean = simpleName.isConstructor

    /** Returns `true` iff this is the name of a static initializer. */
    def isStaticInitializer: Boolean = simpleName.isStaticInitializer

    /** Returns `true` iff this is the name of a class initializer. */
    def isClassInitializer: Boolean = simpleName.isClassInitializer
  }

  object MethodName {
    private val ReflectiveProxyResultTypeRef = ClassRef(ObjectClass)
    private final val ReflectiveProxyResultTypeName = "java.lang.Object"

    def apply(simpleName: SimpleMethodName, paramTypeRefs: List[TypeRef],
        resultTypeRef: TypeRef, isReflectiveProxy: Boolean): MethodName = {
      if ((simpleName.isConstructor || simpleName.isStaticInitializer ||
          simpleName.isClassInitializer) && resultTypeRef != VoidRef) {
        throw new IllegalArgumentException(
            "A constructor or static initializer must have a void result type")
      }
      if (isReflectiveProxy && resultTypeRef != ReflectiveProxyResultTypeRef) {
        throw new IllegalArgumentException(
            "A reflective proxy must have a result type of " +
            ReflectiveProxyResultTypeName)
      }
      new MethodName(simpleName, paramTypeRefs, resultTypeRef,
          isReflectiveProxy)
    }

    // Convenience constructors

    def apply(simpleName: SimpleMethodName, paramTypeRefs: List[TypeRef],
        resultTypeRef: TypeRef): MethodName = {
      apply(simpleName, paramTypeRefs, resultTypeRef, isReflectiveProxy = false)
    }

    def apply(simpleName: String, paramTypeRefs: List[TypeRef],
        resultTypeRef: TypeRef): MethodName = {
      apply(SimpleMethodName(simpleName), paramTypeRefs, resultTypeRef)
    }

    def constructor(paramTypeRefs: List[TypeRef]): MethodName = {
      new MethodName(SimpleMethodName.Constructor, paramTypeRefs, VoidRef,
          isReflectiveProxy = false)
    }

    def reflectiveProxy(simpleName: SimpleMethodName,
        paramTypeRefs: List[TypeRef]): MethodName = {
      apply(simpleName, paramTypeRefs, ReflectiveProxyResultTypeRef,
          isReflectiveProxy = true)
    }

    def reflectiveProxy(simpleName: String,
        paramTypeRefs: List[TypeRef]): MethodName = {
      reflectiveProxy(SimpleMethodName(simpleName), paramTypeRefs)
    }
  }

  /** The full name of a class.
   *
   *  A class name is non-empty sequence of `.`-separated simple names, where
   *  each simple name must be non-empty and can contain any Unicode code
   *  point except `/ . ; [`.
   */
  final class ClassName private (encoded: UTF8String)
      extends Name(encoded) with Comparable[ClassName] {

    type ThisName = ClassName

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: ClassName => equalsName(that)
        case _               => false
      })
    }

    protected def stringPrefix: String = "ClassName"

    def withSuffix(suffix: String): ClassName =
      ClassName(encoded ++ UTF8String(suffix))
  }

  object ClassName {
    def apply(name: UTF8String): ClassName =
      new ClassName(validateEncodedClassName(name))

    def apply(name: String): ClassName =
      ClassName(UTF8String(name))
  }

  // scalastyle:on equals.hash.code

  /** `java.lang.Object`, the root of the class hierarchy. */
  val ObjectClass: ClassName = ClassName("java.lang.Object")

  // Hijacked classes
  val BoxedUnitClass: ClassName = ClassName("java.lang.Void")
  val BoxedBooleanClass: ClassName = ClassName("java.lang.Boolean")
  val BoxedCharacterClass: ClassName = ClassName("java.lang.Character")
  val BoxedByteClass: ClassName = ClassName("java.lang.Byte")
  val BoxedShortClass: ClassName = ClassName("java.lang.Short")
  val BoxedIntegerClass: ClassName = ClassName("java.lang.Integer")
  val BoxedLongClass: ClassName = ClassName("java.lang.Long")
  val BoxedFloatClass: ClassName = ClassName("java.lang.Float")
  val BoxedDoubleClass: ClassName = ClassName("java.lang.Double")
  val BoxedStringClass: ClassName = ClassName("java.lang.String")

  /** The set of all hijacked classes. */
  val HijackedClasses: Set[ClassName] = Set(
      BoxedUnitClass,
      BoxedBooleanClass,
      BoxedCharacterClass,
      BoxedByteClass,
      BoxedShortClass,
      BoxedIntegerClass,
      BoxedLongClass,
      BoxedFloatClass,
      BoxedDoubleClass,
      BoxedStringClass
  )

  /** The class of things returned by `ClassOf` and `GetClass`. */
  val ClassClass: ClassName = ClassName("java.lang.Class")

  /** `java.lang.Cloneable`, which is an ancestor of array classes and is used
   *  by `Clone`.
   */
  val CloneableClass: ClassName = ClassName("java.lang.Cloneable")

  /** `java.io.Serializable`, which is an ancestor of array classes. */
  val SerializableClass: ClassName = ClassName("java.io.Serializable")

  /** The superclass of all throwables.
   *
   *  This is the result type of `WrapAsThrowable` nodes, as well as the input
   *  type of `UnwrapFromThrowable`.
   */
  val ThrowableClass = ClassName("java.lang.Throwable")

  /** The exception thrown by a division by 0. */
  val ArithmeticExceptionClass: ClassName =
    ClassName("java.lang.ArithmeticException")

  /** The exception thrown by an `ArraySelect` that is out of bounds. */
  val ArrayIndexOutOfBoundsExceptionClass: ClassName =
    ClassName("java.lang.ArrayIndexOutOfBoundsException")

  /** The exception thrown by an `Assign(ArraySelect, ...)` where the value cannot be stored. */
  val ArrayStoreExceptionClass: ClassName =
    ClassName("java.lang.ArrayStoreException")

  /** The exception thrown by a `NewArray(...)` with a negative size. */
  val NegativeArraySizeExceptionClass: ClassName =
    ClassName("java.lang.NegativeArraySizeException")

  /** The exception thrown by a variety of nodes for `null` arguments.
   *
   *  - `Apply` and `ApplyStatically` for the receiver,
   *  - `Select` for the qualifier,
   *  - `ArrayLength` and `ArraySelect` for the array,
   *  - `GetClass`, `Clone` and `UnwrapFromException` for their respective only arguments.
   */
  val NullPointerExceptionClass: ClassName =
    ClassName("java.lang.NullPointerException")

  /** The exception thrown by a `BinaryOp.String_charAt` that is out of bounds. */
  val StringIndexOutOfBoundsExceptionClass: ClassName =
    ClassName("java.lang.StringIndexOutOfBoundsException")

  /** The exception thrown by an `AsInstanceOf` that fails. */
  val ClassCastExceptionClass: ClassName =
    ClassName("java.lang.ClassCastException")

  /** The set of classes and interfaces that are ancestors of array classes. */
  private[ir] val AncestorsOfPseudoArrayClass: Set[ClassName] = {
    /* This would logically be defined in Types, but that introduces a cyclic
     * dependency between the initialization of Names and Types.
     */
    Set(ObjectClass, CloneableClass, SerializableClass)
  }

  /** Name of a constructor without argument.
   *
   *  This is notably the signature of constructors of module classes.
   */
  final val NoArgConstructorName: MethodName =
    MethodName.constructor(Nil)

  /** This is used to construct a java.lang.Class. */
  final val ObjectArgConstructorName: MethodName =
    MethodName.constructor(List(ClassRef(ObjectClass)))

  /** Name of the static initializer method. */
  final val StaticInitializerName: MethodName =
    MethodName(SimpleMethodName.StaticInitializer, Nil, VoidRef)

  /** Name of the class initializer method. */
  final val ClassInitializerName: MethodName =
    MethodName(SimpleMethodName.ClassInitializer, Nil, VoidRef)

  /** ModuleID of the default module */
  final val DefaultModuleID: String = "main"

  // ---------------------------------------------------
  // ----- Private helpers for validation of names -----
  // ---------------------------------------------------

  private def throwInvalidEncodedName(encoded: UTF8String): Nothing =
    throw new IllegalArgumentException(s"Invalid name: $encoded")

  private def validateSimpleEncodedName(encoded: UTF8String): UTF8String =
    validateSimpleEncodedName(encoded, 0, encoded.length, openAngleBracketOK = true)

  private def validateSimpleEncodedName(encoded: UTF8String, start: Int,
      end: Int, openAngleBracketOK: Boolean): UTF8String = {

    if (start == end)
      throwInvalidEncodedName(encoded)
    var i = start
    while (i != end) {
      (encoded(i).toInt: @switch) match {
        case '.' | ';' | '[' | '/' =>
          throwInvalidEncodedName(encoded)
        case '<' =>
          if (!openAngleBracketOK)
            throwInvalidEncodedName(encoded)
        case _ =>
          /* This case is hit for other ASCII characters, but also for the
           * leading and continuation bytes of multibyte code points. They are
           * all valid, since an `EncodedName` is already guaranteed to be a
           * valid UTF-8 sequence.
           */
      }
      i += 1
    }

    encoded
  }

  private def validateEncodedClassName(encoded: UTF8String): UTF8String = {
    val len = encoded.length
    var i = 0
    while (i < len) {
      val start = i
      while (i != len && encoded(i) != '.')
        i += 1
      validateSimpleEncodedName(encoded, start, i, openAngleBracketOK = true)
      i += 1 // goes to `len + 1` iff we successfully parsed the last segment
    }

    /* Make sure that there isn't an empty segment at the end. This happens
     * either when `len == 0` (in which case the *only* segment is empty) or
     * when the last byte in `encoded` is a `.` (example: in `java.lang.`).
     */
    if (i == len)
      throwInvalidEncodedName(encoded)

    encoded
  }

}
