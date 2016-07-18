package org.scalajs.testng

import java.lang.annotation.Annotation

import scala.scalajs.js.annotation.JSExportDescendentObjects

@JSExportDescendentObjects
trait TestNGTestBootstrapper {
  def metadata(): TestNGClassMetadata
  def newInstance(): AnyRef
  def invoke(methodName: String): Unit
  def invoke(instance: AnyRef, methodName: String): Unit
}

final class TestNGMethodMetadata(val name: String, annotations: List[Annotation]) {

  def hasTestAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.testng.annotations.Test])

  def hasBeforeAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.testng.annotations.BeforeMethod])

  def hasAfterAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.testng.annotations.AfterMethod])

  def hasBeforeClassAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.testng.annotations.BeforeClass])

  def hasAfterClassAnnotation: Boolean =
    annotations.exists(_.isInstanceOf[org.testng.annotations.AfterClass])

  def getTestAnnotation: Option[org.testng.annotations.Test] =
    annotations.collectFirst { case test: org.testng.annotations.Test => test }

  def hasEnabledTestAnnotation: Boolean = getTestAnnotation.exists(_.enabled)
}

final class TestNGClassMetadata(classAnnotations: List[Annotation],
    moduleAnnotations: List[Annotation], classMethods: List[TestNGMethodMetadata],
    moduleMethods: List[TestNGMethodMetadata]) {

  def testMethods: List[TestNGMethodMetadata] = {
    val tests = classMethods.filter(_.hasTestAnnotation)
    tests.sortBy(_.name)
  }

  def beforeMethod: List[TestNGMethodMetadata] =
    classMethods.filter(_.hasBeforeAnnotation)

  def afterMethod: List[TestNGMethodMetadata] =
    classMethods.filter(_.hasAfterAnnotation)

  def beforeClassMethod: List[TestNGMethodMetadata] =
    moduleMethods.filter(_.hasBeforeClassAnnotation)

  def afterClassMethod: List[TestNGMethodMetadata] =
    moduleMethods.filter(_.hasAfterClassAnnotation)
}
