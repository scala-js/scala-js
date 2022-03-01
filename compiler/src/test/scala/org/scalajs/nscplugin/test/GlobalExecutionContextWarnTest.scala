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

package org.scalajs.nscplugin.test

import org.scalajs.nscplugin.test.util._
import org.scalajs.nscplugin.test.util.VersionDependentUtils.scalaSupportsNoWarn

import org.junit.Assume._
import org.junit.Test

class GlobalExecutionContextWarnTest extends DirectTest with TestHelpers {

  @Test
  def warnOnUsage: Unit = {
    """
    import scala.concurrent.ExecutionContext.global

    object Enclosing {
      global
    }
    """ hasWarns
    """
      |newSource1.scala:5: warning: The global execution context in Scala.js is based on JS Promises (microtasks).
      |Using it may prevent macrotasks (I/O, timers, UI rendering) from running reliably.
      |
      |Unfortunately, there is no way with ECMAScript only to implement a performant
      |macrotask execution context (and hence Scala.js core does not contain one).
      |
      |We recommend you use: https://github.com/scala-js/scala-js-macrotask-executor
      |Please refer to the README.md of that project for more details regarding
      |microtask vs. macrotask execution contexts.
      |
      |If you do not care about macrotask fairness, you can silence this warning by:
      |- Adding @nowarn("cat=other") (Scala >= 2.13.x only)
      |- Setting the -P:scalajs:nowarnGlobalExecutionContext compiler option (Scala < 3.x.y only)
      |- Using scala.scalajs.concurrent.JSExecutionContext.queue
      |  (the implementation of ExecutionContext.global in Scala.js) directly.
      |
      |If you do not care about performance, you can use
      |scala.scalajs.concurrent.QueueExecutionContext.timeouts().
      |It is based on setTimeout which makes it fair but slow (due to clamping).
      |
      |      global
      |      ^
    """
  }

  @Test
  def warnOnImplicitUsage: Unit = {
    """
    import scala.concurrent.ExecutionContext.Implicits.global

    object Enclosing {
      scala.concurrent.Future { }
    }
    """ hasWarns
    """
      |newSource1.scala:5: warning: The global execution context in Scala.js is based on JS Promises (microtasks).
      |Using it may prevent macrotasks (I/O, timers, UI rendering) from running reliably.
      |
      |Unfortunately, there is no way with ECMAScript only to implement a performant
      |macrotask execution context (and hence Scala.js core does not contain one).
      |
      |We recommend you use: https://github.com/scala-js/scala-js-macrotask-executor
      |Please refer to the README.md of that project for more details regarding
      |microtask vs. macrotask execution contexts.
      |
      |If you do not care about macrotask fairness, you can silence this warning by:
      |- Adding @nowarn("cat=other") (Scala >= 2.13.x only)
      |- Setting the -P:scalajs:nowarnGlobalExecutionContext compiler option (Scala < 3.x.y only)
      |- Using scala.scalajs.concurrent.JSExecutionContext.queue
      |  (the implementation of ExecutionContext.global in Scala.js) directly.
      |
      |If you do not care about performance, you can use
      |scala.scalajs.concurrent.QueueExecutionContext.timeouts().
      |It is based on setTimeout which makes it fair but slow (due to clamping).
      |
      |      scala.concurrent.Future { }
      |                              ^
    """
  }

  @Test
  def noWarnIfSelectivelyDisabled: Unit = {
    assumeTrue(scalaSupportsNoWarn)

    """
    import scala.annotation.nowarn
    import scala.concurrent.ExecutionContext.global

    object Enclosing {
      global: @nowarn("cat=other")
    }
    """.hasNoWarns()
  }

  @Test
  def noWarnQueue: Unit = {
    /* Test that JSExecutionContext.queue does not warn for good measure.
     * We explicitly say it doesn't so we want to notice if it does.
     */

    """
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

    object Enclosing {
      scala.concurrent.Future { }
    }
    """.hasNoWarns()
  }

}
