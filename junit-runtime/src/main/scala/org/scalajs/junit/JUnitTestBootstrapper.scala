package org.scalajs.junit

import java.lang.annotation.Annotation

import org.junit.FixMethodOrder

import scala.scalajs.reflect.annotation._

@EnableReflectiveInstantiation
trait JUnitTestBootstrapper {
  def metadata(): JUnitClassMetadata
  def newInstance(): AnyRef
  def invoke(methodName: String): Unit
  def invoke(instance: AnyRef, methodName: String): Unit
}

final class JUnitMethodMetadata(val name: String, annotations: List[Annotation]) {

  def hasTestAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.junit.Test])

  def hasBeforeAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.junit.Before])

  def hasAfterAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.junit.After])

  def hasBeforeClassAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.junit.BeforeClass])

  def hasAfterClassAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.junit.AfterClass])

  def getTestAnnotation: Option[org.junit.Test] =
    annotations.collectFirst { case test: org.junit.Test => test }

  def getIgnoreAnnotation: Option[org.junit.Ignore] =
    annotations.collectFirst { case ign: org.junit.Ignore => ign }
}

final class JUnitClassMetadata(classAnnotations: List[Annotation],
    moduleAnnotations: List[Annotation], classMethods: List[JUnitMethodMetadata],
    moduleMethods: List[JUnitMethodMetadata]) {

  def testMethods: List[JUnitMethodMetadata] = {
    val fixMethodOrderAnnotation = getFixMethodOrderAnnotation
    val methodSorter = fixMethodOrderAnnotation.value
    val tests = classMethods.filter(_.hasTestAnnotation)
    tests.sortWith((a, b) => methodSorter.comparator.lt(a.name, b.name))
  }

  def beforeMethod: List[JUnitMethodMetadata] =
    classMethods.filter(_.hasBeforeAnnotation)

  def afterMethod: List[JUnitMethodMetadata] =
    classMethods.filter(_.hasAfterAnnotation)

  def beforeClassMethod: List[JUnitMethodMetadata] =
    moduleMethods.filter(_.hasBeforeClassAnnotation)

  def afterClassMethod: List[JUnitMethodMetadata] =
    moduleMethods.filter(_.hasAfterClassAnnotation)

  def getFixMethodOrderAnnotation: FixMethodOrder = {
    classAnnotations.collectFirst {
      case fmo: FixMethodOrder => fmo
    }.getOrElse(new FixMethodOrder)
  }
}
