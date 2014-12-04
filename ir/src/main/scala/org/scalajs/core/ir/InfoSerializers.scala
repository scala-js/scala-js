/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import java.io._

import Infos._

object InfoSerializers {

  /** Scala.js IR File Magic Number
   *
   *    CA FE : first part of magic number of Java class files
   *    4A 53 : "JS" in ASCII
   *
   */
  final val IRMagicNumber = 0xCAFE4A53

  def serialize(stream: OutputStream, classInfo: ClassInfo): Unit = {
    new Serializer().serialize(stream, classInfo)
  }

  def deserializeRoughInfo(stream: InputStream): RoughClassInfo = {
    deserializeVersionRoughInfo(stream)._2
  }

  def deserializeFullInfo(stream: InputStream): ClassInfo = {
    deserializeVersionFullInfo(stream)._2
  }

  def deserializeVersionRoughInfo(stream: InputStream): (String, RoughClassInfo) = {
    new Deserializer(stream).deserializeRough()
  }

  def deserializeVersionFullInfo(stream: InputStream): (String, ClassInfo) = {
    new Deserializer(stream).deserializeFull()
  }

  private final class Serializer {
    def serialize(stream: OutputStream, classInfo: ClassInfo): Unit = {
      val s = new DataOutputStream(stream)

      def writeSeq[A](seq: Seq[A])(writeElem: A => Unit): Unit = {
        s.writeInt(seq.size)
        seq.foreach(writeElem)
      }

      def writeStrings(seq: Seq[String]): Unit =
        writeSeq(seq)(s.writeUTF(_))

      // Write the Scala.js IR magic number
      s.writeInt(IRMagicNumber)

      // Write the Scala.js Version
      s.writeUTF(ScalaJSVersions.binaryEmitted)

      import classInfo._
      s.writeUTF(name)
      s.writeUTF(encodedName)
      s.writeBoolean(isExported)
      s.writeInt(ancestorCount)
      s.writeByte(ClassKind.toByte(kind))
      s.writeUTF(superClass)
      writeStrings(ancestors)
      s.writeInt(optimizerHints.bits)

      def writeMethodInfo(methodInfo: MethodInfo): Unit = {
        import methodInfo._
        s.writeUTF(encodedName)
        s.writeBoolean(isStatic)
        s.writeBoolean(isAbstract)
        s.writeBoolean(isExported)
        writeSeq(methodsCalled.toSeq) {
          case (cls, callees) => s.writeUTF(cls); writeStrings(callees)
        }
        writeSeq(methodsCalledStatically.toSeq) {
          case (cls, callees) => s.writeUTF(cls); writeStrings(callees)
        }
        writeSeq(staticMethodsCalled.toSeq) {
          case (cls, callees) => s.writeUTF(cls); writeStrings(callees)
        }
        writeStrings(instantiatedClasses)
        writeStrings(accessedModules)
        writeStrings(accessedClassData)
        s.writeInt(optimizerHints.bits)
      }

      writeSeq(methods)(writeMethodInfo(_))

      s.flush()
    }
  }

  private final class Deserializer(stream: InputStream) {
    private[this] val input = new DataInputStream(stream)

    def readList[A](readElem: => A): List[A] =
      List.fill(input.readInt())(readElem)

    def readStrings(): List[String] =
      readList(input.readUTF())

    def deserializeRough(): (String, RoughClassInfo) = {
      val version = readHeader()

      import input._
      val name = readUTF()
      val encodedName = readUTF()
      val isExported = readBoolean()
      val ancestorCount = readInt()
      val info = RoughClassInfo(name, encodedName, isExported, ancestorCount)

      (version, info)
    }

    def deserializeFull(): (String, ClassInfo) = {
      val version = readHeader()

      import input._

      val name = readUTF()
      val encodedName = readUTF()
      val isExported = readBoolean()
      val ancestorCount = readInt()
      val kind = ClassKind.fromByte(readByte())
      val superClass = readUTF()
      val ancestors = readList(readUTF())

      val optimizerHints =
        if (version == "0.5.0" || version == "0.5.2") OptimizerHints.empty
        else new OptimizerHints(readInt())

      def readMethod(): MethodInfo = {
        val encodedName = readUTF()
        val isStatic = readBoolean()
        val isAbstract = readBoolean()
        val isExported = readBoolean()
        val methodsCalled = readList(readUTF() -> readStrings()).toMap
        val methodsCalledStatically = readList(readUTF() -> readStrings()).toMap
        val staticMethodsCalled = readList(readUTF() -> readStrings()).toMap
        val instantiatedClasses = readStrings()
        val accessedModules = readStrings()
        val accessedClassData = readStrings()
        val optimizerHints = new OptimizerHints(readInt())
        MethodInfo(encodedName, isStatic, isAbstract, isExported,
            methodsCalled, methodsCalledStatically, staticMethodsCalled,
            instantiatedClasses, accessedModules, accessedClassData,
            optimizerHints)
      }

      val methods = readList(readMethod())

      val info = ClassInfo(name, encodedName, isExported, ancestorCount, kind,
          superClass, ancestors, optimizerHints, methods)

      (version, info)
    }

    /** Reads the Scala.js IR header and verifies the version compatibility.
     *  Returns the emitted binary version.
     */
    def readHeader(): String = {
      // Check magic number
      if (input.readInt() != IRMagicNumber)
        throw new IOException("Not a Scala.js IR file")

      // Check that we support this version of the IR
      val version = input.readUTF()
      val supported = ScalaJSVersions.binarySupported
      if (!supported.contains(version)) {
        throw new IOException(
            s"This version ($version) of Scala.js IR is not supported. " +
            s"Supported versions are: ${supported.mkString(", ")}")
      }

      version
    }
  }
}
