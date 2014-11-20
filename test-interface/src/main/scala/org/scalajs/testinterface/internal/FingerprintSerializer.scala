package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.Dynamic.{literal => lit}

import sbt.testing._

object FingerprintSerializer {

  def serialize(fp: Fingerprint): js.Dynamic = fp match {
    case fp: AnnotatedFingerprint => lit(
        fpType = "AnnotatedFingerprint",
        isModule = fp.isModule,
        annotationName = fp.annotationName)
    case fp: SubclassFingerprint => lit(
        fpType = "SubclassFingerprint",
        isModule = fp.isModule,
        superclassName = fp.superclassName,
        requireNoArgConstructor = fp.requireNoArgConstructor)
    case _ =>
      throw new IllegalArgumentException(
        s"Unknown Fingerprint type: ${fp.getClass}")
  }

  def deserialize(obj: js.Dynamic): Fingerprint = {
    obj.fpType.asInstanceOf[String] match {
      case "AnnotatedFingerprint" =>
        new DeserializedAnnotatedFingerprint(
            obj.isModule.asInstanceOf[Boolean],
            obj.annotationName.asInstanceOf[String])
      case "SubclassFingerprint" =>
        new DeserializedSubclassFingerprint(
            obj.isModule.asInstanceOf[Boolean],
            obj.superclassName.asInstanceOf[String],
            obj.requireNoArgConstructor.asInstanceOf[Boolean])
      case tpe =>
        throw new IllegalArgumentException(s"Unknown Fingerprint type: $tpe")
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
