/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.nodejs

import scala.collection.immutable

import org.scalajs.jsenv._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._

import java.io.{ Console => _, _ }

class NodeJSEnv(config: NodeJSEnv.Config) extends AbstractNodeJSEnv {

  def this() = this(NodeJSEnv.Config())

  protected def vmName: String = "Node.js"

  protected def executable: String = config.executable

  override protected def args: immutable.Seq[String] = config.args

  override protected def env: Map[String, String] = config.env

  // TODO Our Build wants this to be public, but it does not seem clean
  override def wantSourceMap: Boolean = config.sourceMap

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

object NodeJSEnv {
  final class Config private (
      val executable: String,
      val args: List[String],
      val env: Map[String, String],
      val sourceMap: Boolean
  ) {
    private def this() = {
      this(
          executable = "node",
          args = Nil,
          env = Map.empty,
          sourceMap = true
      )
    }

    def withExecutable(executable: String): Config =
      copy(executable = executable)

    def withArgs(args: List[String]): Config =
      copy(args = args)

    def withEnv(env: Map[String, String]): Config =
      copy(env = env)

    def withSourceMap(sourceMap: Boolean): Config =
      copy(sourceMap = sourceMap)

    private def copy(
        executable: String = executable,
        args: List[String] = args,
        env: Map[String, String] = env,
        sourceMap: Boolean = sourceMap
    ): Config = {
      new Config(executable, args, env, sourceMap)
    }
  }

  object Config {
    /** Returns a default configuration for a [[NodeJSEnv]].
     *
     *  The defaults are:
     *
     *  - `executable`: `"node"`
     *  - `args`: `Nil`
     *  - `env`: `Map.empty`
     *  - `sourceMap`: `true`
     */
    def apply(): Config = new Config()
  }
}
