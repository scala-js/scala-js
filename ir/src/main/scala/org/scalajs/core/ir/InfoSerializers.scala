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

  def deserialize(stream: InputStream): ClassInfo = {
    deserializeWithVersion(stream)._2
  }

  def deserializeWithVersion(stream: InputStream): (String, ClassInfo) = {
    new Deserializer(stream).deserialize()
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
      s.writeUTF(encodedName)
      s.writeBoolean(isExported)
      s.writeByte(ClassKind.toByte(kind))
      s.writeUTF(superClass.getOrElse(""))
      writeStrings(interfaces)

      def writeMethodInfo(methodInfo: MethodInfo): Unit = {
        import methodInfo._

        def writePerClassStrings(m: Map[String, List[String]]): Unit = {
          writeSeq(m.toSeq) {
            case (cls, items) => s.writeUTF(cls); writeStrings(items)
          }
        }

        s.writeUTF(encodedName)
        s.writeBoolean(isStatic)
        s.writeBoolean(isAbstract)
        s.writeBoolean(isExported)
        writePerClassStrings(staticFieldsRead)
        writePerClassStrings(staticFieldsWritten)
        writePerClassStrings(methodsCalled)
        writePerClassStrings(methodsCalledStatically)
        writePerClassStrings(staticMethodsCalled)
        writeStrings(instantiatedClasses)
        writeStrings(accessedModules)
        writeStrings(usedInstanceTests)
        writeStrings(accessedClassData)
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

    def deserialize(): (String, ClassInfo) = {
      val version = readHeader()

      import input._

      val useHacks065 =
        Set("0.6.0", "0.6.3", "0.6.4", "0.6.5").contains(version)
      val useHacks0614 =
        useHacks065 || Set("0.6.6", "0.6.8", "0.6.13", "0.6.14").contains(version)

      val encodedName = readUTF()
      val isExported = readBoolean()
      val kind = ClassKind.fromByte(readByte())
      val superClass0 = readUTF()
      val superClass = if (superClass0 == "") None else Some(superClass0)
      val interfaces = readList(readUTF())

      def readMethod(): MethodInfo = {
        def readPerClassStrings(): Map[String, List[String]] =
          readList(readUTF() -> readStrings()).toMap

        val encodedName = readUTF()
        val isStatic = readBoolean()
        val isAbstract = readBoolean()
        val isExported = readBoolean()
        val staticFieldsRead =
          if (useHacks0614) Map.empty[String, List[String]]
          else readPerClassStrings()
        val staticFieldsWritten =
          if (useHacks0614) Map.empty[String, List[String]]
          else readPerClassStrings()
        val methodsCalled = readPerClassStrings()
        val methodsCalledStatically = readPerClassStrings()
        val staticMethodsCalled = readPerClassStrings()
        val instantiatedClasses = readStrings()
        val accessedModules = readStrings()
        val usedInstanceTests = readStrings()
        val accessedClassData = readStrings()
        MethodInfo(encodedName, isStatic, isAbstract, isExported,
            staticFieldsRead, staticFieldsWritten,
            methodsCalled, methodsCalledStatically, staticMethodsCalled,
            instantiatedClasses, accessedModules, usedInstanceTests,
            accessedClassData)
      }

      val methods0 = readList(readMethod())
      val methods = if (useHacks065) {
        methods0.filter(m => !Definitions.isReflProxyName(m.encodedName))
      } else {
        methods0
      }

      val info = ClassInfo(encodedName, isExported, kind,
          superClass, interfaces, methods)

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
        throw new IRVersionNotSupportedException(version, supported,
            s"This version ($version) of Scala.js IR is not supported. " +
            s"Supported versions are: ${supported.mkString(", ")}")
      }

      version
    }
  }
}
