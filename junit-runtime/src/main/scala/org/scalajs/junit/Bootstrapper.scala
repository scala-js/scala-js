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

import scala.concurrent.Future
import scala.util.Try

import scala.scalajs.reflect.annotation._

/** Scala.js internal JUnit bootstrapper.
 *
 *  This class is public due to implementation details. Only the junit compiler
 *  plugin may generate classes inheriting from it.
 *
 *  Relying on this trait directly is unspecified behavior.
 */
@EnableReflectiveInstantiation
trait Bootstrapper {
  def beforeClass(): Unit
  def afterClass(): Unit
  def before(instance: AnyRef): Unit
  def after(instance: AnyRef): Unit

  def tests(): Array[TestMetadata]
  def invokeTest(instance: AnyRef, name: String): Future[Try[Unit]]

  def newInstance(): AnyRef
}

/** Scala.js internal JUnit test metadata
 *
 *  This class is public due to implementation details. Only the junit compiler
 *  plugin may create instances of it.
 *
 *  Relying on this class directly is unspecified behavior.
 */
final class TestMetadata(
    val name: String,
    val ignored: Boolean,
    val annotation: org.junit.Test
)
