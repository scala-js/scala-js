/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import sbt.testing._

import org.scalajs.core.tools.json._

private[testadapter] object FingerprintSerializers {

  implicit object FingerprintSerializer extends JSONSerializer[Fingerprint] {
    def serialize(fp: Fingerprint): JSON = {
      val bld = new JSONObjBuilder()
      fp match {
        case fp: AnnotatedFingerprint => bld
          .fld("fpType", "AnnotatedFingerprint")
          .fld("isModule", fp.isModule)
          .fld("annotationName", fp.annotationName)
        case fp: SubclassFingerprint => bld
          .fld("fpType", "SubclassFingerprint")
          .fld("isModule", fp.isModule)
          .fld("superclassName", fp.superclassName)
          .fld("requireNoArgConstructor", fp.requireNoArgConstructor)
        case _ =>
          throw new IllegalArgumentException(
              s"Unknown Fingerprint type: ${fp.getClass}")
      }
      bld.toJSON
    }
  }

  implicit object FingerprintDeserializer extends JSONDeserializer[Fingerprint] {
    def deserialize(x: JSON): Fingerprint = {
      val obj = new JSONObjExtractor(x)
      obj.fld[String]("fpType") match {
        case "AnnotatedFingerprint" =>
          new DeserializedAnnotatedFingerprint(
              obj.fld[Boolean]("isModule"),
              obj.fld[String]("annotationName"))
        case "SubclassFingerprint" =>
          new DeserializedSubclassFingerprint(
              obj.fld[Boolean]("isModule"),
              obj.fld[String]("superclassName"),
              obj.fld[Boolean]("requireNoArgConstructor"))
        case tpe =>
          throw new IllegalArgumentException(s"Unknown Fingerprint type: $tpe")
      }
    }
  }

  final class DeserializedAnnotatedFingerprint(
      val isModule: Boolean,
      val annotationName: String
  ) extends AnnotatedFingerprint

  final class DeserializedSubclassFingerprint(
      val isModule: Boolean,
      val superclassName: String,
      val requireNoArgConstructor: Boolean
  ) extends SubclassFingerprint

}
