package org.scalajs.junit

import sbt.testing.AnnotatedFingerprint

object JUnitFingerprint extends AnnotatedFingerprint {
  override def annotationName(): String = "org.junit.Test"

  override def isModule(): Boolean = false
}
