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

class BeforeAndAfterTestAssertions extends JUnitTest with SuccessFrameworkArgs {

  override val expectedTotal: Int = 1

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testStartedOutput("test"),
        testFinishedOutput("test"),
        successEvent,
        testRunFinishedOutput,
        done
    )
  }
}
