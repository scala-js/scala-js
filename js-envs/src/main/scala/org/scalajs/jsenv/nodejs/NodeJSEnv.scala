/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.nodejs

import org.scalajs.jsenv._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._

import java.io.{ Console => _, _ }

class NodeJSEnv private (
    @deprecatedName('nodejsPath)
    override protected val executable: String, // override val for bin compat
    @deprecatedName('addArgs)
    args: Seq[String],
    @deprecatedName('addEnv)
    env: Map[String, String],
    sourceMap: Boolean)
    extends AbstractNodeJSEnv(executable, args, env, sourceMap) {

  def this(
      @deprecatedName('nodejsPath)
      executable: String = "node",
      @deprecatedName('addArgs)
      args: Seq[String] = Seq.empty,
      @deprecatedName('addEnv)
      env: Map[String, String] = Map.empty) = {
    this(executable, args, env, sourceMap = true)
  }

  def withSourceMap(sourceMap: Boolean): NodeJSEnv =
    new NodeJSEnv(executable, args, env, sourceMap)

  protected def vmName: String = "Node.js"

  override def jsRunner(files: Seq[VirtualJSFile]): JSRunner =
    new NodeRunner(files)

  override def asyncRunner(files: Seq[VirtualJSFile]): AsyncJSRunner =
    new AsyncNodeRunner(files)

  override def comRunner(files: Seq[VirtualJSFile]): ComJSRunner =
    new ComNodeRunner(files)

  protected class NodeRunner(files: Seq[VirtualJSFile])
      extends ExtRunner(files) with AbstractBasicNodeRunner

  protected class AsyncNodeRunner(files: Seq[VirtualJSFile])
      extends AsyncExtRunner(files) with AbstractBasicNodeRunner

  protected class ComNodeRunner(files: Seq[VirtualJSFile])
      extends AsyncNodeRunner(files) with NodeComJSRunner

  protected trait AbstractBasicNodeRunner extends AbstractNodeRunner {

    // Send code to Stdin
    override protected def sendVMStdin(out: OutputStream): Unit = {
      /* Do not factor this method out into AbstractNodeRunner or when mixin in
       * the traits it would use AbstractExtRunner.sendVMStdin due to
       * linearization order.
       */
      sendJS(getJSFiles(), out)
    }
  }

}
