/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package scalajs

import Properties.{ copyrightString, msilLibPath }
import java.io.File.pathSeparator
import scala.tools.nsc.interactive.{ RefinedBuildManager, SimpleBuildManager }
import scala.tools.nsc.io.AbstractFile

/** The main object a variant of the compiler targeting JavaScript
 *
 *  @author Sébastien Doeraene
 */
object Main extends Driver with EvalLoop {
  // We should extend scala.tools.nsc.Main, but that's an object :-(

  val versionString = "version 0.1"

  override val versionMsg = "Scala.js compiler " +
    versionString + " -- " +
    copyrightString

  override def newCompiler(): Global =
    if (settings.Yrangepos.value) new Global(settings, reporter) with JSGlobal with interactive.RangePositions
    else new Global(settings, reporter) with JSGlobal

  // --- Start copy and paste from scala.tools.nsc.Main

  def resident(compiler: Global) {
    loop { line =>
      val args = line.split(' ').toList
      val command = new CompilerCommand(args, new Settings(scalacError))
      compiler.reporter.reset()
      new compiler.Run() compile command.files
    }
  }

  override def processSettingsHook(): Boolean =
    if (settings.Yidedebug.value) {
      settings.Xprintpos.value = true
      settings.Yrangepos.value = true
      val compiler = new interactive.Global(settings, reporter)
      import compiler.{ reporter => _, _ }

      val sfs = command.files map getSourceFile
      val reloaded = new interactive.Response[Unit]
      askReload(sfs, reloaded)

      reloaded.get.right.toOption match {
        case Some(ex) => reporter.cancelled = true // Causes exit code to be non-0
        case None => reporter.reset() // Causes other compiler errors to be ignored
      }
      askShutdown
      false
    }
    else if (settings.Ybuilderdebug.value != "none") {
      def fileSet(files : List[String]) = Set.empty ++ (files map AbstractFile.getFile)

      val buildManager = settings.Ybuilderdebug.value match {
        case "simple" => new SimpleBuildManager(settings)
        case _ =>
          // Trick to silence the deprecation warning on RefinedBuildManager
          @deprecated("ignore", "2.10.0")
          def newRefinedBuildManager() = new RefinedBuildManager(settings)
          newRefinedBuildManager()
      }
      buildManager.addSourceFiles(fileSet(command.files))

      // enter resident mode
      loop { line =>
        val args = line.split(' ').toList
        val command = new CompilerCommand(args.toList, settings)
        buildManager.update(fileSet(command.files), Set.empty)
      }
      false
    }
    else {
      if (settings.target.value == "msil")
        msilLibPath foreach (x => settings.assemrefs.value += (pathSeparator + x))
      true
    }

  override def doCompile(compiler: Global) {
    if (settings.resident.value)
      resident(compiler)
    else super.doCompile(compiler)
  }

  // --- End copy and paste from scala.tools.nsc.Main
}
