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

package org.scalajs.junit

import scala.scalajs.reflect.annotation._

@EnableReflectiveInstantiation
trait Bootstrapper {
  def beforeClass(): Unit
  def afterClass(): Unit
  def before(instance: AnyRef): Unit
  def after(instance: AnyRef): Unit

  def tests(): Array[TestMetadata]
  def invokeTest(instance: AnyRef, name: String): Unit

  def newInstance(): AnyRef
}

final class TestMetadata(
    val name: String,
    val ignored: Boolean,
    val annotation: org.junit.Test
)
