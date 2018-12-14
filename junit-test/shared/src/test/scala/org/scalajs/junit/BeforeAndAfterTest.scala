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

import org.junit._
import org.scalajs.junit.utils._

object BeforeAndAfterTest {
  @BeforeClass def beforeClass(): Unit = ()
  @AfterClass def afterClass(): Unit = ()
}

class BeforeAndAfterTest {
  @Before def before(): Unit = ()
  @After def after(): Unit = ()
  @Test def test(): Unit = ()
}

class BeforeAndAfterTestAssertions extends JUnitTest
