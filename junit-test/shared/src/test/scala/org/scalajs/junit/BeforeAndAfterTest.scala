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

class BeforeAndAfterTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder =
    builder.success("test")
}
