package scala.scalajs.runtime

import scala.scalajs.js

import StackTrace.JSStackTraceElem

/** Information about the JavaScript environment Scala.js runs in.
 *
 *  Holds configuration for the Scala.js internals and should not be used
 *  directly (could be retrieved via [[runtime.environmentInfo]]).
 *
 *  This facade type serves as a documentation on what aspects of Scala.js can
 *  be influenced through environment options.
 *
 *  Upon startup, Scala.js checks whether the name <code>__ScalaJSEnv</code> is
 *  defined in its scope (and references an object). If so, it uses it as
 *  environment info.
 *  Missing, non-optional fields (according to this facade type) are initialized
 *  to default values, optional fields are kept as in the original object.
 *  Finally, [[js.Object.freeze]] is called on the object to avoid modification.
 *
 *  @groupname envInfo Scala.js environment configuration
 *  @groupprio envInfo 1
 */
@js.native
trait EnvironmentInfo extends js.Object {

  /** The global JavaScript scope (corresponds to js.Dynamic.global)
   *
   *  @group envInfo
   */
  def global: js.Dynamic = js.native

  /** The scope for Scala.js exports (i.e. objects and classes)
   *
   *  @group envInfo
   */
  def exportsNamespace: js.Dynamic = js.native

  // Can't link to java.lang.Runtime.exit - #1969
  /** The function that is called by `java.lang.Runtime.exit`
   *
   *  @group envInfo
   */
  def exitFunction: js.UndefOr[js.Function1[Int, Nothing]] = js.native

  /** Method used to source map JavaScript stack traces
   *
   *  @group envInfo
   */
  def sourceMapper: js.UndefOr[js.Function1[ // scalastyle:ignore
    js.Array[JSStackTraceElem], js.Array[JSStackTraceElem]]] = js.native

  /** Dictionary of system properties to add to java.lang.System.getProperties()
   *
   *  @group envInfo
   */
  def javaSystemProperties: js.UndefOr[js.Dictionary[String]]
}
