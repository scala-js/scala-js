package scala.scalajs.sbtplugin.env

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.env._
import scala.scalajs.tools.logging._

import scala.scalajs.sbtplugin.JSUtils._

import java.io.{ Console => _, _ }
import scala.io.Source

abstract class ExternalJSEnv(
  final protected val additionalArgs: Seq[String],
  final protected val additionalEnv:  Seq[String]) extends JSEnv {

  import ExternalJSEnv._

  /** Printable name of this VM */
  protected def vmName: String

  /** Command to execute (on shell) for this VM */
  protected def executable: String

  /** JS files used to setup VM */
  protected def initFiles(args: RunJSArgs): Seq[VirtualJSFile] = Nil

  /** Sends required data to VM Stdin (can throw) */
  protected def sendVMStdin(args: RunJSArgs, out: OutputStream): Unit = {}

  /** Fire up an instance of the VM and send js input to it.
    * Don't care about exceptions. Calling code will catch and display
    * an error message.
    */
  def runJS(classpath: CompleteClasspath, code: VirtualJSFile,
    logger: Logger, console: JSConsole): Option[String] = {

    val runJSArgs = RunJSArgs(classpath, code, logger, console)

    val vmInst = startVM(runJSArgs)

    // Prepare and send input to VM
    val out = vmInst.getOutputStream()
    try { sendVMStdin(runJSArgs, out) }
    finally { out.close() }

    // We are now executing. Pipe stdout to console
    pipeToConsole(vmInst.getInputStream(), console)

    // We are probably done (stdin is closed). Report any errors
    val errSrc = Source.fromInputStream(vmInst.getErrorStream(), "UTF-8")
    try { errSrc.getLines.foreach(err => logger.error(err)) }
    finally { errSrc.close }

    // Make sure we are done.
    vmInst.waitFor()

    // Get return value and return
    val retVal = vmInst.exitValue

    if (retVal == 0) None
    else Some(s"$vmName exited with code $retVal")

  }

  /** send a bunch of JS files to an output stream */
  final protected def sendJS(files: Seq[VirtualJSFile],
      out: OutputStream): Unit = {
    val writer = new BufferedWriter(new OutputStreamWriter(out, "UTF-8"))
    try sendJS(files, writer)
    finally writer.close()
  }

  /** send a bunch of JS files to a writer */
  final protected def sendJS(files: Seq[VirtualJSFile], out: Writer): Unit =
    files.foreach { writeJSFile(_, out) }

  /** write a single JS file to a writer using an include fct if appropriate */
  protected def writeJSFile(file: VirtualJSFile, writer: Writer) = {
    file match {
      case file: FileVirtualJSFile =>
        val fname = toJSstr(file.file.getAbsolutePath)
        writer.write(s"require($fname);\n")
      case _ =>
        writer.write(file.content)
        writer.write('\n')
    }
  }

  /** pipe lines from input stream to JSConsole */
  final protected def pipeToConsole(in: InputStream, console: JSConsole) = {
    val source = Source.fromInputStream(in, "UTF-8")
    try { source.getLines.foreach(console.log _) }
    finally { source.close() }
  }

  protected def startVM(args: RunJSArgs): Process = {
    val vmArgs = getVMArgs(args)
    val vmEnv  = getVMEnv(args)

    val allArgs = (executable +: vmArgs).toArray
    sys.runtime.exec(allArgs, vmEnv.toArray)
  }

  /** VM arguments excluding executable. Override to adapt.
   *  Overrider is responsible to add additionalArgs.
   */
  protected def getVMArgs(args: RunJSArgs): Seq[String] = additionalArgs

  /** VM environment. Override to adapt.
   *  Override is responsible to add additionalEnv
   */
  protected def getVMEnv(args: RunJSArgs): Seq[String] = additionalEnv

  /** Get files that are a library (i.e. that do not run anything) */
  protected def getLibJSFiles(args: RunJSArgs): Seq[VirtualJSFile] =
    initFiles(args) ++ args.classpath.allCode

  /** Get all files that are passed to VM (libraries and code) */
  protected def getJSFiles(args: RunJSArgs): Seq[VirtualJSFile] =
    getLibJSFiles(args) :+ args.code

}

object ExternalJSEnv {

  case class RunJSArgs(
      classpath: CompleteClasspath,
      code: VirtualJSFile,
      logger: Logger,
      console: JSConsole)

}
