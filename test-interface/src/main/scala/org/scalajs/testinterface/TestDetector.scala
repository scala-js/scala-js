package org.scalajs.testinterface

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.reflect.Reflect

import org.scalajs.testinterface.internal.TaskDefSerializer

import sbt.testing._

/** Fetches test definitions and frameworks. */
private[scalajs] object TestDetector {
  def detectTests(): Seq[(Framework, Seq[TaskDef])] = {
    import RawDefinitions._

    val taskDefs = definedTests.map(TaskDefSerializer.deserialize _)
    val frameworks = testFrameworkNames.flatMap(tryLoadFramework).toList

    for {
      framework <- frameworks
    } yield {
      val fingerprints = framework.fingerprints()
      val eligibleTaskDefs = taskDefs.filter(taskDef =>
        fingerprints.exists(fingerprintMatches(_, taskDef.fingerprint)))
      (framework, eligibleTaskDefs.toSeq)
    }
  }

  private def tryLoadFramework(names: js.Array[String]): Option[Framework] = {
    def tryLoadFromReflect(name: String): Option[Framework] = {
      Reflect.lookupInstantiatableClass(name).collect {
        case clazz if classOf[Framework].isAssignableFrom(clazz.runtimeClass) =>
          clazz.newInstance().asInstanceOf[Framework]
      }
    }

    def tryLoadFromExportsNamespace(name: String): Option[Framework] = {
      val exportsNamespace =
        scala.scalajs.runtime.linkingInfo.envInfo.exportsNamespace

      val parts = name.split('.')
      val ctor = parts.foldLeft[js.UndefOr[js.Dynamic]](exportsNamespace) {
        (parent, name) => parent.map(_.selectDynamic(name))
      }

      ctor.map(js.Dynamic.newInstance(_)()).collect {
        case framework: Framework => framework
      }.toOption
    }

    def tryLoad(name: String): Option[Framework] =
      tryLoadFromReflect(name).orElse(tryLoadFromExportsNamespace(name))

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
    val definedTests: js.Array[js.Dynamic] = js.native
    val testFrameworkNames: js.Array[js.Array[String]] = js.native
  }
}
