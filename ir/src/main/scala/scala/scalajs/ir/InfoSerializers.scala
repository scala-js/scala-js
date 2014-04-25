/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import java.io._

import Infos._

object InfoSerializers {
  def serialize(stream: OutputStream, classInfo: ClassInfo): Unit = {
    new Serializer().serialize(stream, classInfo)
  }

  def deserializeRoughInfo(stream: InputStream): RoughClassInfo = {
    new Deserializer(stream).deserializeRough()
  }

  def deserializeFullInfo(stream: InputStream): ClassInfo = {
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

      import classInfo._
      s.writeUTF(name)
      s.writeUTF(encodedName)
      s.writeBoolean(isExported)
      s.writeInt(ancestorCount)
      s.writeByte(ClassKind.toByte(kind))
      s.writeUTF(superClass)
      writeStrings(ancestors)

      def writeMethodInfo(methodInfo: MethodInfo): Unit = {
        import methodInfo._
        s.writeUTF(encodedName)
        s.writeBoolean(isAbstract)
        s.writeBoolean(isExported)
        writeSeq(calledMethods.toSeq) {
          case (caller, callees) => s.writeUTF(caller); writeStrings(callees)
        }
        writeSeq(calledMethodsStatic.toSeq) {
          case (caller, callees) => s.writeUTF(caller); writeStrings(callees)
        }
        writeStrings(instantiatedClasses)
        writeStrings(accessedModules)
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

    def deserializeRough(): RoughClassInfo = {
      import input._
      val name = readUTF()
      val encodedName = readUTF()
      val isExported = readBoolean()
      val ancestorCount = readInt()
      RoughClassInfo(name, encodedName, isExported, ancestorCount)
    }

    def deserializeFull(): ClassInfo = {
      import input._

      val name = readUTF()
      val encodedName = readUTF()
      val isExported = readBoolean()
      val ancestorCount = readInt()
      val kind = ClassKind.fromByte(readByte())
      val superClass = readUTF()
      val ancestors = readList(readUTF())

      def readMethod(): MethodInfo = {
        val encodedName = readUTF()
        val isAbstract = readBoolean()
        val isExported = readBoolean()
        val calledMethods = readList(readUTF() -> readStrings()).toMap
        val calledMethodsStatic = readList(readUTF() -> readStrings()).toMap
        val instantiatedClasses = readStrings()
        val accessedModules = readStrings()
        val accessedClassData = readStrings()
        MethodInfo(encodedName, isAbstract, isExported,
            calledMethods, calledMethodsStatic,
            instantiatedClasses, accessedModules, accessedClassData)
      }

      val methods = readList(readMethod())

      ClassInfo(name, encodedName, isExported, ancestorCount, kind,
          superClass, ancestors, methods)
    }
  }
}
