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

package java.lang

import scala.scalajs.js

/** Information about the JavaScript environment Scala.js runs in.
 *
 *  Holds configuration for the Scala.js internals and should not be used
 *  directly (could be retrieved via [[EnvironmentInfo.envInfo]]).
 *
 *  This facade type serves as a documentation on what aspects of the Scala.js
 *  implementation of `java.lang` can be influenced through environment
 *  options.
 *
 *  If there is a variable named `__ScalaJSEnv` in Scala.js' scope (and it
 *  references an object), it is used as the value of
 *  [[EnvironmentInfo.envInfo]]. Otherwise, the latter is `undefined`.
 *
 *  @groupname envInfo Scala.js environment configuration
 *  @groupprio envInfo 1
 */
private[lang] sealed trait EnvironmentInfo extends js.Object {

  // Can't link to java.lang.Runtime.exit - #1969
  /** The function that is called by `java.lang.Runtime.exit`
   *
   *  @group envInfo
   */
  def exitFunction: js.UndefOr[js.Function1[Int, Nothing]]

  /** Dictionary of system properties to add to java.lang.System.getProperties()
   *
   *  @group envInfo
   */
  def javaSystemProperties: js.UndefOr[js.Dictionary[String]]
}

private[lang] object EnvironmentInfo {
  /** The value of the `__ScalaJSEnv` variable, if it exists. */
  val envInfo: js.UndefOr[EnvironmentInfo] = {
    import js.Dynamic.{global => g}

    if (js.typeOf(g.__ScalaJSEnv) == "object" && g.__ScalaJSEnv != null)
      g.__ScalaJSEnv.asInstanceOf[EnvironmentInfo]
    else
      js.undefined
  }
}
