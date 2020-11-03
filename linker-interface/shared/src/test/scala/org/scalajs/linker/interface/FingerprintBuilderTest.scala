/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.interface

import org.junit.Test
import org.junit.Assert._

import Fingerprint._

class FingerprintBuilderTest {
  @Test
  def noFields(): Unit = {
    val fingerprint = new FingerprintBuilder("Test0").build()
    assertEquals("Test0()", fingerprint)
  }

  @Test
  def oneField(): Unit = {
    val fingerprint = new FingerprintBuilder("Test1")
      .addField("test1", true)
      .build()
    assertEquals("Test1(test1=true)", fingerprint)
  }

  @Test
  def multipleFields(): Unit = {
    val fingerprint = new FingerprintBuilder("Test2")
      .addField("test1", Some(1): Option[Int])
      .addField("test2", None: Option[Int])
      .addField("test3", List.empty[Int])
      .addField("test4", List(1, 2, 3))
      .build()
    assertEquals("Test2(test1=Some(1),test2=None,test3=List(),test4=List(1,2,3))", fingerprint)
  }
}
