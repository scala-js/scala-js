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

import scala.annotation.switch

import java.nio.charset.StandardCharsets.UTF_8

import Types._
import java.nio.charset.CodingErrorAction
import java.nio.CharBuffer
import java.nio.charset.CharacterCodingException

object Names {
  type EncodedName = Array[Byte]

  private final val ConstructorSimpleEncodedName: EncodedName =
    "<init>".map(_.toByte).toArray

  private final val ClassConstructorSimpleEncodedName: EncodedName =
    "<clinit>".map(_.toByte).toArray

  // scalastyle:off equals.hash.code
  // we define hashCode() once in Name, but equals() separately in its subclasses

  sealed abstract class Name(protected val encoded: EncodedName) {
    type ThisName <: Name

    // Eagerly compute the hash code
    private val _hashCode = scala.util.hashing.MurmurHash3.bytesHash(encoded)

    /** Returns the underlying encoded name.
     *
     *  This method directly returns the underlying array, which is mutable,
     *  although names must stay immutable.
     *
     *  Callers *must not* mutate the returned array!
     */
    final def unsafeEncoded: EncodedName = encoded

    override def hashCode(): Int = _hashCode

    protected final def equalsName(that: ThisName): Boolean = {
      this._hashCode == that._hashCode && // fail fast on different hash codes
      java.util.Arrays.equals(this.encoded, that.encoded)
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
      decodeStringName(encoded)

    override def toString(): String =
      stringPrefix + "<" + nameString + ">"
  }

  /** The name of a local variable or capture parameter.
   *
   *  Local names must be non-empty, and can contain any Unicode code point
   *  except `/ . ; [`.
   */
  final class LocalName private (encoded: EncodedName)
      extends Name(encoded) with Comparable[LocalName] {

    type ThisName = LocalName

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: LocalName => equalsName(that)
        case _               => false
      })
    }

    protected def stringPrefix: String = "LocalName"

    final def withPrefix(prefix: LocalName): LocalName = {
      new LocalName(
          concatEncodedNames(prefix.unsafeEncoded, this.unsafeEncoded))
    }

    final def withPrefix(prefix: String): LocalName = {
      new LocalName(
          concatEncodedNames(encodeStringName(prefix), this.unsafeEncoded))
    }

    final def withSuffix(suffix: LocalName): LocalName = {
      new LocalName(
          concatEncodedNames(this.unsafeEncoded, suffix.unsafeEncoded))
    }

    final def withSuffix(suffix: String): LocalName = {
      new LocalName(
          concatEncodedNames(this.unsafeEncoded, encodeStringName(suffix)))
    }
  }

  object LocalName {
    def apply(name: EncodedName): LocalName =
      new LocalName(validateSimpleEncodedName(name))

    def apply(name: String): LocalName =
      LocalName(encodeStringName(name))

    def unsafeCreate(encoded: EncodedName): LocalName =
      new LocalName(encoded)
  }

  /** The name of the label of a `Labeled` block.
   *
   *  Label names must be non-empty, and can contain any Unicode code point
   *  except `/ . ; [`.
   */
  final class LabelName private (encoded: EncodedName)
      extends Name(encoded) with Comparable[LabelName] {

    type ThisName = LabelName

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: LabelName => equalsName(that)
        case _               => false
      })
    }

    protected def stringPrefix: String = "LabelName"

    final def withSuffix(suffix: LabelName): LabelName = {
      new LabelName(
          concatEncodedNames(this.unsafeEncoded, suffix.unsafeEncoded))
    }

    final def withSuffix(suffix: String): LabelName = {
      new LabelName(
          concatEncodedNames(this.unsafeEncoded, encodeStringName(suffix)))
    }
  }

  object LabelName {
    def apply(name: EncodedName): LabelName =
      new LabelName(validateSimpleEncodedName(name))

    def apply(name: String): LabelName =
      LabelName(encodeStringName(name))

    def unsafeCreate(encoded: EncodedName): LabelName =
      new LabelName(encoded)
  }

  /** The name of a field.
   *
   *  Field names must be non-empty, and can contain any Unicode code point
   *  except `/ . ; [`.
   */
  final class FieldName private (encoded: EncodedName)
      extends Name(encoded) with Comparable[FieldName] {

    type ThisName = FieldName

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: FieldName => equalsName(that)
        case _               => false
      })
    }

    protected def stringPrefix: String = "FieldName"

    final def withSuffix(suffix: String): FieldName = {
      new FieldName(
          concatEncodedNames(this.unsafeEncoded, encodeStringName(suffix)))
    }

    final def toLocalName: LocalName =
      LocalName.unsafeCreate(unsafeEncoded)
  }

  object FieldName {
    def apply(name: EncodedName): FieldName =
      new FieldName(validateSimpleEncodedName(name))

    def apply(name: String): FieldName =
      FieldName(encodeStringName(name))

    def unsafeCreate(encoded: EncodedName): FieldName =
      new FieldName(encoded)
  }

  /** The simple name of a method (excluding its signature).
   *
   *  Simple names must be non-empty, and can contain any Unicode code point
   *  except `/ . ; [`. In addition, they must not contain the code point `<`
   *  unless they are the string `<init>` or the string `<clinit>`.
   */
  final class SimpleMethodName private (encoded: EncodedName)
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
    def isConstructor: Boolean = this == ConstructorSimpleName

    /** Returns `true` iff this is the name of a static initializer. */
    def isStaticInitializer: Boolean = this == StaticInitializerSimpleName
  }

  object SimpleMethodName {
    def apply(name: EncodedName): SimpleMethodName =
      new SimpleMethodName(validateEncodedSimpleMethodName(name))

    def apply(name: String): SimpleMethodName =
      SimpleMethodName(encodeStringName(name))

    def unsafeCreate(encoded: EncodedName): SimpleMethodName =
      new SimpleMethodName(encoded)
  }

  val ConstructorSimpleName: SimpleMethodName =
    SimpleMethodName("<init>")

  val StaticInitializerSimpleName: SimpleMethodName =
    SimpleMethodName("<clinit>")

  /** The full name of a method, including its simple name and its signature.
   */
  final class MethodName private (val simpleName: SimpleMethodName,
      val paramTypeRefs: List[TypeRef], val resultTypeRef: Option[TypeRef]) {

    import MethodName._

    private val _hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      var acc = 1270301484 // "MethodName".hashCode()
      acc = mix(acc, simpleName.##)
      acc = mix(acc, paramTypeRefs.##)
      acc = mixLast(acc, resultTypeRef.##)
      finalizeHash(acc, 3)
    }

    private[this] val kind = {
      if (simpleName.isConstructor)
        ConstructorKind
      else if (simpleName.isStaticInitializer)
        StaticInitializerKind
      else if (resultTypeRef.isDefined)
        NormalKind
      else
        ReflectiveProxyKind
    }

    override def equals(that: Any): Boolean = {
      (this eq that.asInstanceOf[AnyRef]) || (that match {
        case that: MethodName =>
          this._hashCode == that._hashCode && // fail fast on different hash codes
          this.simpleName == that.simpleName &&
          this.paramTypeRefs == that.paramTypeRefs &&
          this.resultTypeRef == that.resultTypeRef
        case _ =>
          false
      })
    }

    override def hashCode(): Int = _hashCode

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
      }

      builder.append(simpleName.nameString)
      for (paramTypeRef <- paramTypeRefs) {
        builder.append(';')
        appendTypeRef(paramTypeRef)
      }
      builder.append(';')
      for (resTypeRef <- resultTypeRef)
        appendTypeRef(resTypeRef)
      builder.toString()
    }

    override def toString(): String =
      "MethodName<" + nameString + ">"

    def displayName: String = {
      simpleName.nameString + "(" +
      paramTypeRefs.map(_.displayName).mkString(",") + ")" +
      resultTypeRef.fold("")(_.displayName)
    }

    /** Returns `true` iff this is the name of an instance constructor. */
    def isConstructor: Boolean = kind == ConstructorKind

    /** Returns `true` iff this is the name of a static initializer. */
    def isStaticInitializer: Boolean = kind == StaticInitializerKind

    /** Returns `true` iff this is the name of a reflective proxy. */
    def isReflectiveProxy: Boolean = kind == ReflectiveProxyKind
  }

  object MethodName {
    private final val NormalKind = 0
    private final val ConstructorKind = 1
    private final val StaticInitializerKind = 2
    private final val ReflectiveProxyKind = 3

    def apply(simpleName: SimpleMethodName, paramTypeRefs: List[TypeRef],
        resultTypeRef: Option[TypeRef]): MethodName = {
      if ((simpleName.isConstructor || simpleName.isStaticInitializer) &&
          resultTypeRef.isDefined) {
        throw new IllegalArgumentException(
            "A constructor or static initializer must not have a result type")
      }
      new MethodName(simpleName, paramTypeRefs, resultTypeRef)
    }

    // Convenience constructors

    def apply(simpleName: String, paramTypeRefs: List[TypeRef],
        resultTypeRef: TypeRef): MethodName = {
      apply(SimpleMethodName(simpleName), paramTypeRefs, Some(resultTypeRef))
    }

    def apply(simpleName: String, paramTypeRefs: List[TypeRef]): MethodName =
      apply(SimpleMethodName(simpleName), paramTypeRefs, None)

    def constructor(paramTypeRefs: List[TypeRef]): MethodName =
      apply(ConstructorSimpleName, paramTypeRefs, None)
  }

  /** The full name of a class.
   *
   *  A class name is non-empty sequence of `.`-separated simple names, where
   *  each simple name must be non-empty and can contain any Unicode code
   *  point except `/ . ; [`.
   */
  final class ClassName private (encoded: EncodedName)
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
      ClassName(concatEncodedNames(unsafeEncoded, encodeStringName(suffix)))
  }

  object ClassName {
    def apply(name: EncodedName): ClassName =
      new ClassName(validateEncodedClassName(name))

    def apply(name: String): ClassName =
      ClassName(encodeStringName(name))

    def unsafeCreate(encoded: EncodedName): ClassName =
      new ClassName(encoded)
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

  /** Name of a constructor without argument.
   *
   *  This is notably the signature of constructors of module classes.
   */
  final val NoArgConstructorName: MethodName =
    MethodName.constructor(Nil)

  /** Name of the static initializer method. */
  final val StaticInitializerName: MethodName =
    MethodName(StaticInitializerSimpleName, Nil, None)

  // --------------------------------------------------------------------------
  // ----- Private helpers for validation, encoding and decoding of names -----
  // --------------------------------------------------------------------------

  // --- Validation of encoded names ---

  private def throwInvalidEncodedName(encoded: EncodedName): Nothing = {
    throw new IllegalArgumentException(
        "Invalid encoded name: " + new String(encoded, UTF_8))
  }

  private def validateSimpleEncodedName(encoded: EncodedName): EncodedName =
    validateSimpleEncodedName(encoded, 0, encoded.length, openAngleBracketOK = true)

  private def validateSimpleEncodedName(encoded: EncodedName, start: Int,
      end: Int, openAngleBracketOK: Boolean): EncodedName = {

    if (start == end)
      throwInvalidEncodedName(encoded)
    var i = start
    while (i != end) {
      val b = encoded(i).toInt
      if (b >= 0) {
        // single-byte code point, ASCII repertoire
        (b: @switch) match {
          case '.' | ';' | '[' | '/' =>
            throwInvalidEncodedName(encoded)
          case '<' =>
            if (!openAngleBracketOK)
              throwInvalidEncodedName(encoded)
          case _ =>
        }
        i += 1
      } else {
        // multi-byte code point
        i += validateMultibyteCodePointAndGetByteLen(encoded, end, i, b)
      }
    }

    encoded
  }

  private def validateEncodedClassName(encoded: EncodedName): EncodedName = {
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

  private def validateEncodedSimpleMethodName(encoded: EncodedName): EncodedName = {
    val len = encoded.length
    if (len == 0)
      throwInvalidEncodedName(encoded)

    // Validate the '<' characters
    if (encoded(0) == '<') {
      // Must be either '<init>' or '<clinit>'
      len match {
        case 6 if java.util.Arrays.equals(encoded, ConstructorSimpleEncodedName) =>
          // ok
        case 8 if java.util.Arrays.equals(encoded, ClassConstructorSimpleEncodedName) =>
          // ok
        case _ =>
          throwInvalidEncodedName(encoded)
      }
    } else {
      // Normal method name
      validateSimpleEncodedName(encoded, 0, len, openAngleBracketOK = false)
    }

    encoded
  }

  private def validateMultibyteCodePointAndGetByteLen(encoded: EncodedName,
      end: Int, i: Int, b1: Int): Int = {

    @inline def isInvalidNextByte(b: Int): Boolean =
      (b & 0xc0) != 0x80

    if ((b1 & 0xe0) == 0xc0) { // 110xxxxx
      if (i > end - 2) {
        throwInvalidEncodedName(encoded)
      } else {
        val b2 = encoded(i + 1) & 0xff
        if (isInvalidNextByte(b2)) {
          throwInvalidEncodedName(encoded)
        } else {
          val cp = (((b1 & 0x1f) << 6) | (b2 & 0x3f))
          if (cp >= 0x80)
            2
          else
            throwInvalidEncodedName(encoded)
        }
      }
    } else if ((b1 & 0xf0) == 0xe0) { // 1110xxxx
      if (i > end - 3) {
        throwInvalidEncodedName(encoded)
      } else {
        val b2 = encoded(i + 1) & 0xff
        val b3 = encoded(i + 2) & 0xff
        if (isInvalidNextByte(b2) || isInvalidNextByte(b3)) {
          throwInvalidEncodedName(encoded)
        } else {
          val cp = (((b1 & 0xf) << 12) | ((b2 & 0x3f) << 6) | (b3 & 0x3f))
          if (cp >= 0x800 && !Character.isSurrogate(cp.toChar))
            3
          else
            throwInvalidEncodedName(encoded)
        }
      }
    } else if ((b1 & 0xf8) == 0xf0) { // 11110xxx
      if (i > end - 4) {
        throwInvalidEncodedName(encoded)
      } else {
        val b2 = encoded(i + 1) & 0xff
        val b3 = encoded(i + 2) & 0xff
        val b4 = encoded(i + 3) & 0xff
        if (isInvalidNextByte(b2) || isInvalidNextByte(b3) || isInvalidNextByte(b4)) {
          throwInvalidEncodedName(encoded)
        } else {
          val cp = (((b1 & 0x7) << 18) | ((b2 & 0x3f) << 12) |
              ((b3 & 0x3f) << 6) | (b4 & 0x3f))
          if (cp >= 0x10000 && cp <= Character.MAX_CODE_POINT)
            4
          else
            throwInvalidEncodedName(encoded)
        }
      }
    } else {
      throwInvalidEncodedName(encoded)
    }
  }

  // --- Encoding of strings into names ---

  private def encodeStringName(name: String): EncodedName = {
    // scalastyle:off return
    val len = name.length()
    if (len == 0)
      throwEncodingError(name)

    /* We optimistically assume that all characters are ASCII, and backtrack if
     * we find a non-ASCII character.
     */
    val result = new Array[Byte](len)
    var i = 0
    while (i != len) {
      val c = name.charAt(i).toInt
      if ((c & 0x7f) != c)
        return encodeStringNameWithNonASCII(name)
      result(i) = c.toByte
      i += 1
    }
    result
    // scalastyle:on return
  }

  private def encodeStringNameWithNonASCII(name: String): EncodedName = {
    // Note: a UTF-8 encoder can never encounter an "unmappable" character
    val encoder = UTF_8.newEncoder().onMalformedInput(CodingErrorAction.REPORT)
    try {
      val outputBuffer = encoder.encode(CharBuffer.wrap(name))
      val result = new Array[Byte](outputBuffer.remaining())
      outputBuffer.get(result)
      result
    } catch {
      case _: CharacterCodingException =>
        throwEncodingError(name)
    }
  }

  private def throwEncodingError(name: String): Nothing =
    throw new IllegalArgumentException(s"Not a valid name: " + name)

  // --- Decoding of names into strings ---

  private def decodeStringName(encoded: EncodedName): String = {
    // scalastyle:off return
    /* We optimistically assume that all characters are single-byte (i.e., in
     * the ASCII repertoire), and fall back to a full UTF-8 decoder if we find
     * a multi-byte character.
     */
    val len = encoded.length
    val result = new Array[Char](len)
    var i = 0
    while (i != len) {
      val b = encoded(i)
      if (b < 0)
        return new String(encoded, UTF_8)
      result(i) = (b & 0xff).toChar
      i += 1
    }
    new String(result)
    // scalastyle:on return
  }

  // --- Manipulation of encoded names ---

  private def concatEncodedNames(encoded1: EncodedName,
      encoded2: EncodedName): EncodedName = {
    val len1 = encoded1.length
    val len2 = encoded2.length
    val result = java.util.Arrays.copyOf(encoded1, len1 + len2)
    System.arraycopy(encoded2, 0, result, len1, len2)
    result
  }

}
