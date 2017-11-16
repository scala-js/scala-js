package org.scalajs.testinterface

import java.io._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.reflect.Reflect

import org.scalajs.testcommon.Serializer

import sbt.testing._

/** Fetches test definitions and frameworks. */
private[scalajs] object TestDetector {
  def detectTests(): Seq[(Framework, Seq[TaskDef])] = {
    val taskDefs = Serializer.deserialize[List[TaskDef]](
        RawDefinitions.definedTests)

    val frameworkNames = Serializer.deserialize[List[List[String]]](
        RawDefinitions.testFrameworkNames)

    for {
      nameAlternatives <- frameworkNames
      framework <- tryLoadFramework(nameAlternatives).toList
    } yield {
      val fingerprints = framework.fingerprints()
      val eligibleTaskDefs = taskDefs.filter(taskDef =>
        fingerprints.exists(fingerprintMatches(_, taskDef.fingerprint)))
      (framework, eligibleTaskDefs.toSeq)
    }
  }

  private def tryLoadFramework(names: List[String]): Option[Framework] = {
    def tryLoad(name: String): Option[Framework] = {
      Reflect.lookupInstantiatableClass(name).collect {
        case clazz if classOf[Framework].isAssignableFrom(clazz.runtimeClass) =>
          clazz.newInstance().asInstanceOf[Framework]
      }
    }

    names.toStream.map(tryLoad).flatten.headOption
  }

  // Copied from sbt.TestFramework
  private def fingerprintMatches(a: Fingerprint, b: Fingerprint): Boolean = {
    (a, b) match {
      case (a: SubclassFingerprint, b: SubclassFingerprint) =>
        a.isModule == b.isModule && a.superclassName == b.superclassName

      case (a: AnnotatedFingerprint, b: AnnotatedFingerprint) =>
        a.isModule == b.isModule && a.annotationName == b.annotationName

      case _ => false
    }
  }

  @js.native
  @JSGlobalScope
  private object RawDefinitions extends js.Object {
    val definedTests: String = js.native
    val testFrameworkNames: String = js.native
  }
}
