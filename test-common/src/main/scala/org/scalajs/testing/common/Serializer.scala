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

package org.scalajs.testing.common

import sbt.testing._
import java.io._

private[testing] trait Serializer[T] {
  def serialize(x: T, out: Serializer.SerializeState): Unit
  def deserialize(in: Serializer.DeserializeState): T
}

private[testing] object Serializer {
  /* Serialization states. Serialization is currently stateless, in the sense
   * that the same value will always get serialized to the same bytes, no matter
   * where it is in the bytestream.
   * In the future we might want to deduplicate things like package prefixes,
   * since a lot of data seems to be redundant.
   */
  final class SerializeState private[Serializer] (val out: DataOutputStream)
      extends AnyVal {
    def write[T](t: T)(implicit s: Serializer[T]): Unit = s.serialize(t, this)
  }

  final class DeserializeState private[Serializer] (val in: DataInputStream)
      extends AnyVal {
    def read[T]()(implicit s: Serializer[T]): T = s.deserialize(this)
  }

  // Methods to actually perform serialization and deserialization.

  def serialize[T](t: T, out: DataOutputStream)(implicit s: Serializer[T]): Unit = {
    s.serialize(t, new SerializeState(out))
  }

  def deserialize[T](in: DataInputStream)(implicit s: Serializer[T]): T = {
    s.deserialize(new DeserializeState(in))
  }

  def serialize[T: Serializer](t: T): String =
    withOutputStream(Serializer.serialize(t, _))

  def deserialize[T: Serializer](s: String): T =
    withInputStream(s)(Serializer.deserialize[T](_))

  @inline
  def withInputStream[T](s: String)(body: DataInputStream => T): T = {
    val bytes = s.toArray.map(_.toByte)
    val in = new DataInputStream(new ByteArrayInputStream(bytes))
    try body(in)
    finally in.close()
  }

  @inline
  def withOutputStream(body: DataOutputStream => Unit): String = {
    val byteOut = new ByteArrayOutputStream()
    val dataOut = new DataOutputStream(byteOut)

    try body(dataOut)
    finally dataOut.close()

    new String(byteOut.toByteArray.map(b => (b & 0xff).toChar))
  }

  implicit object BooleanSerializer extends Serializer[Boolean] {
    def serialize(x: Boolean, out: SerializeState): Unit = out.out.writeBoolean(x)
    def deserialize(in: DeserializeState): Boolean = in.in.readBoolean()
  }

  implicit object ByteSerializer extends Serializer[Byte] {
    def serialize(x: Byte, out: SerializeState): Unit = out.out.writeByte(x)
    def deserialize(in: DeserializeState): Byte = in.in.readByte()
  }

  implicit object IntSerializer extends Serializer[Int] {
    def serialize(x: Int, out: SerializeState): Unit = out.out.writeInt(x)
    def deserialize(in: DeserializeState): Int = in.in.readInt()
  }

  implicit object LongSerializer extends Serializer[Long] {
    def serialize(x: Long, out: SerializeState): Unit = out.out.writeLong(x)
    def deserialize(in: DeserializeState): Long = in.in.readLong()
  }

  implicit object StringSerializer extends Serializer[String] {
    def serialize(x: String, out: SerializeState): Unit = {
      // Modified version of writeUTF to support strings longer than Short.MaxValue (#3667)
      out.out.writeInt(x.length)

      import out.out.write

      for (i <- 0 until x.length()) {
        val c = x.charAt(i)
        if (c <= 0x7f && c >= 0x01) {
          write(c)
        } else if (c < 0x0800) {
          write((c >> 6) | 0xc0)
          write((c & 0x3f) | 0x80)
        } else {
          write((c >> 12) | 0xe0)
          write(((c >> 6) & 0x3f) | 0x80)
          write((c & 0x3f) | 0x80)
        }
      }
    }

    def deserialize(in: DeserializeState): String = {
      import in.in.readByte

      val chars = Array.fill(in.in.readInt()) {
        val a = readByte()

        if ((a & 0x80) == 0x00) { // 0xxxxxxx
          a.toChar
        } else if ((a & 0xe0) == 0xc0) { // 110xxxxx
          val b = readByte()

          require((b & 0xc0) == 0x80) // 10xxxxxx

          (((a & 0x1f) << 6) | (b & 0x3f)).toChar
        } else if ((a & 0xf0) == 0xe0) { // 1110xxxx
          val b = readByte()
          val c = readByte()

          require((b & 0xc0) == 0x80) // 10xxxxxx
          require((c & 0xc0) == 0x80) // 10xxxxxx

          (((a & 0x0f) << 12) | ((b & 0x3f) << 6) | (c & 0x3f)).toChar
        } else {
          throw new IllegalArgumentException(s"bad byte: $a")
        }
      }

      new String(chars)
    }
  }

  implicit object UnitSerializer extends Serializer[Unit] {
    def serialize(x: Unit, out: SerializeState): Unit = ()
    def deserialize(in: DeserializeState): Unit = ()
  }

  implicit def listSerializer[T: Serializer]: Serializer[List[T]] = {
    new Serializer[List[T]] {
      def serialize(x: List[T], out: SerializeState): Unit = {
        out.write(x.size)
        x.foreach(out.write(_))
      }

      def deserialize(in: DeserializeState): List[T] =
        List.fill(in.read[Int]())(in.read[T]())
    }
  }

  implicit def optionSerializer[T: Serializer]: Serializer[Option[T]] = {
    new Serializer[Option[T]] {
      def serialize(x: Option[T], out: SerializeState): Unit = {
        out.write(x.isDefined)
        x.foreach(out.write(_))
      }

      def deserialize(in: DeserializeState): Option[T] = {
        if (in.read[Boolean]()) Some(in.read[T]())
        else None
      }
    }
  }

  implicit object StackTraceElementSerializer extends Serializer[StackTraceElement] {
    def serialize(x: StackTraceElement, out: SerializeState): Unit = {
      out.write(x.getClassName())
      out.write(x.getMethodName())
      out.write(Option(x.getFileName()))
      out.write(x.getLineNumber())
    }

    def deserialize(in: DeserializeState): StackTraceElement = {
      new StackTraceElement(in.read[String](), in.read[String](),
          in.read[Option[String]]().orNull, in.read[Int]())
    }
  }

  implicit object ThrowableSerializer extends Serializer[Throwable] {
    def serialize(x: Throwable, out: SerializeState): Unit = {
      out.write(Option(x.getMessage()))
      out.write(x.toString())
      out.write(x.getStackTrace().toList)
      out.write(Option(x.getCause()))
    }

    def deserialize(in: DeserializeState): Throwable = {
      val msg = in.read[Option[String]]().orNull
      val toStr = in.read[String]()
      val trace = in.read[List[StackTraceElement]]()
      val cause = in.read[Option[Throwable]]()

      val res = new Throwable(msg, cause.orNull) {
        override def toString(): String = toStr
      }

      res.setStackTrace(trace.toArray)

      res
    }
  }

  implicit object FingerprintSerializer extends Serializer[Fingerprint] {
    // Type tags.
    private val Annotated: Byte = 1
    private val Subclass: Byte = 2

    def serialize(fp: Fingerprint, out: SerializeState): Unit = fp match {
      case fp: AnnotatedFingerprint =>
        out.write(Annotated)
        out.write(fp.isModule())
        out.write(fp.annotationName())
      case fp: SubclassFingerprint =>
        out.write(Subclass)
        out.write(fp.isModule())
        out.write(fp.superclassName())
        out.write(fp.requireNoArgConstructor())
      case _ =>
        throw new IllegalArgumentException(
            s"Unknown Fingerprint type: ${fp.getClass()}")
    }

    def deserialize(in: DeserializeState): Fingerprint = in.read[Byte]() match {
      case Annotated =>
        new AnnotatedFingerprint {
          val isModule: Boolean = in.read[Boolean]()
          val annotationName: String = in.read[String]()
        }

      case Subclass =>
        new SubclassFingerprint {
          val isModule: Boolean = in.read[Boolean]()
          val superclassName: String = in.read[String]()
          val requireNoArgConstructor: Boolean = in.read[Boolean]()
        }

      case t =>
        throw new IOException(s"Unknown Fingerprint type: $t")
    }
  }

  implicit object SelectorSerializer extends Serializer[Selector] {
    // Type tags.
    private val Suite: Byte = 1
    private val Test: Byte = 2
    private val NestedSuite: Byte = 3
    private val NestedTest: Byte = 4
    private val TestWildcard: Byte = 5

    def serialize(sel: Selector, out: SerializeState): Unit = sel match {
      case sel: SuiteSelector => out.write(Suite)

      case sel: TestSelector =>
        out.write(Test)
        out.write(sel.testName())

      case sel: NestedSuiteSelector =>
        out.write(NestedSuite)
        out.write(sel.suiteId())

      case sel: NestedTestSelector =>
        out.write(NestedTest)
        out.write(sel.suiteId())
        out.write(sel.testName())

      case sel: TestWildcardSelector =>
        out.write(TestWildcard)
        out.write(sel.testWildcard())

      case _ =>
        throw new IllegalArgumentException(
            s"Unknown Selector type: ${sel.getClass()}")
    }

    def deserialize(in: DeserializeState): Selector = in.read[Byte]() match {
      case Suite        => new SuiteSelector()
      case Test         => new TestSelector(in.read[String]())
      case NestedSuite  => new NestedSuiteSelector(in.read[String]())
      case NestedTest   => new NestedTestSelector(in.read[String](), in.read[String]())
      case TestWildcard => new TestWildcardSelector(in.read[String]())
      case t            => throw new IOException(s"Unknown Selector type: $t")
    }
  }

  implicit object TaskDefSerializer extends Serializer[TaskDef] {
    def serialize(x: TaskDef, out: SerializeState): Unit = {
      out.write(x.fullyQualifiedName())
      out.write(x.fingerprint())
      out.write(x.explicitlySpecified())
      out.write(x.selectors().toList)
    }

    def deserialize(in: DeserializeState): TaskDef = {
      new TaskDef(in.read[String](), in.read[Fingerprint](), in.read[Boolean](),
          in.read[List[Selector]]().toArray)
    }
  }

  implicit object StatusSerializer extends Serializer[Status] {
    def serialize(x: Status, out: SerializeState): Unit = out.write(x.ordinal)

    def deserialize(in: DeserializeState): Status = {
      val values = Status.values()
      val ord = in.read[Int]()
      if (ord < 0 || ord >= values.size)
        throw new IOException(s"Got bad status ordinal: $ord")
      values(ord)
    }
  }

  implicit object OptionalThrowableSerializer extends Serializer[OptionalThrowable] {
    def serialize(x: OptionalThrowable, out: SerializeState): Unit = {
      out.write(x.isDefined())
      if (x.isDefined())
        out.write(x.get())
    }

    def deserialize(in: DeserializeState): OptionalThrowable = {
      if (in.read[Boolean]()) new OptionalThrowable(in.read[Throwable]())
      else new OptionalThrowable()
    }
  }

  implicit object EventSerializer extends Serializer[Event] {
    def serialize(x: Event, out: SerializeState): Unit = {
      out.write(x.fullyQualifiedName())
      out.write(x.fingerprint())
      out.write(x.selector())
      out.write(x.status())
      out.write(x.throwable())
      out.write(x.duration())
    }

    def deserialize(in: DeserializeState): Event = new Event {
      val fullyQualifiedName: String = in.read[String]()
      val fingerprint: Fingerprint = in.read[Fingerprint]()
      val selector: Selector = in.read[Selector]()
      val status: Status = in.read[Status]()
      val throwable: OptionalThrowable = in.read[OptionalThrowable]()
      val duration: Long = in.read[Long]()
    }
  }
}
