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

package org.scalajs.jsenv.nodejs

import org.scalajs.jsenv._

import org.scalajs.core.ir.Utils.escapeJS

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.core.tools.logging._

import java.io.{ Console => _, _ }

class NodeJSEnv(config: NodeJSEnv.Config)
    extends AbstractNodeJSEnv(config.executable, config.args, config.env,
        config.sourceMap) {

  def this() = this(NodeJSEnv.Config())

  @deprecated("Use the overload with a NodeJSEnv.Config.", "0.6.18")
  def this(
      @deprecatedName('nodejsPath)
      executable: String = "node",
      @deprecatedName('addArgs)
      args: Seq[String] = Seq.empty,
      @deprecatedName('addEnv)
      env: Map[String, String] = Map.empty) = {
    this(NodeJSEnv.Config().withExecutable(executable).withArgs(args.toList).withEnv(env))
  }

  @deprecated("Use the overloaded constructor with a NodeJSEnv.Config.",
      "0.6.18")
  def withSourceMap(sourceMap: Boolean): NodeJSEnv =
    new NodeJSEnv(config.withSourceMap(sourceMap))

  protected def vmName: String = "Node.js"

  // For binary compatibility
  override protected val executable: String = config.executable

  override def jsRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): JSRunner = {
    new NodeRunner(libs, code)
  }

  override def asyncRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): AsyncJSRunner = {
    new AsyncNodeRunner(libs, code)
  }

  override def comRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): ComJSRunner = {
    new ComNodeRunner(libs, code)
  }

  protected class NodeRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile)
      extends ExtRunner(libs, code) with AbstractBasicNodeRunner

  protected class AsyncNodeRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile)
      extends AsyncExtRunner(libs, code) with AbstractBasicNodeRunner

  protected class ComNodeRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile)
      extends AsyncNodeRunner(libs, code) with NodeComJSRunner

  protected trait AbstractBasicNodeRunner extends AbstractNodeRunner {

    /** Libraries are loaded via require in Node.js */
    override protected def getLibJSFiles(): Seq[VirtualJSFile] = {
      initFiles() ++
      customInitFiles() ++
      libs.map(requireLibrary)
    }

    /** Rewrites a library virtual file to a require statement if possible */
    protected def requireLibrary(dep: ResolvedJSDependency): VirtualJSFile = {
      dep.info.commonJSName.fold(dep.lib) { varname =>
        val fname = dep.lib.name
        libCache.materialize(dep.lib)
        new MemVirtualJSFile(s"require-$fname").withContent(
          s"""$varname = require("${escapeJS(fname)}");"""
        )
      }
    }

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
