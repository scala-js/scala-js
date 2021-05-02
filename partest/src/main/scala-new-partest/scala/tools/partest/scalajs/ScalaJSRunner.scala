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

package scala.tools.partest.scalajs

import scala.tools.partest.nest
import scala.tools.partest.nest.{AbstractRunner, DirectCompiler, TestInfo}

class ScalaJSRunner(testInfo: ScalaJSTestInfo, suiteRunner: AbstractRunner,
    options: ScalaJSPartestOptions)
    extends nest.Runner(testInfo, suiteRunner) {

  override def newCompiler = {
    new DirectCompiler(this) with ScalaJSDirectCompiler
  }

  override def extraJavaOptions = {
    super.extraJavaOptions ++ Seq(
        s"-Dscalajs.partest.optMode=${options.optMode.id}",
        s"-Dscalajs.partest.compliantSems=${testInfo.compliantSems.mkString(",")}"
    )
  }
}
