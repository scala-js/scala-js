package org.scalajs.jsenv

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.jsdep.ResolvedJSDependency

import java.io.{ Console => _, _ }
import scala.io.Source

import scala.concurrent.{Future, Promise}
import scala.util.Try

abstract class ExternalJSEnv(
  final protected val additionalArgs: Seq[String],
  final protected val additionalEnv:  Map[String, String]) extends AsyncJSEnv {

  import ExternalJSEnv._

  def name: String = s"ExternalJSEnv for $vmName"

  /** Printable name of this VM */
  protected def vmName: String

  /** Command to execute (on shell) for this VM */
  protected def executable: String

  /** Custom initialization scripts. */
  protected def customInitFiles(): Seq[VirtualJSFile] = Nil

  protected class AbstractExtRunner(
      protected val libs: Seq[ResolvedJSDependency],
      protected val code: VirtualJSFile) extends JSInitFiles {

    private[this] var _logger: Logger = _
    private[this] var _console: JSConsole = _

    protected def logger: Logger = _logger
    protected def console: JSConsole = _console

    protected def setupLoggerAndConsole(logger: Logger, console: JSConsole) = {
      require(_logger == null && _console == null)
      _logger = logger
      _console = console
    }

    /** Custom initialization scripts, defined by the environment. */
    final protected def customInitFiles(): Seq[VirtualJSFile] =
      ExternalJSEnv.this.customInitFiles()

    /** Sends required data to VM Stdin (can throw) */
    protected def sendVMStdin(out: OutputStream): Unit = {}

    /** VM arguments excluding executable. Override to adapt.
     *  Overrider is responsible to add additionalArgs.
     */
    protected def getVMArgs(): Seq[String] = additionalArgs

    /** VM environment. Override to adapt.
     *
     *  Default is `sys.env` and [[additionalEnv]]
     */
    protected def getVMEnv(): Map[String, String] =
      sys.env ++ additionalEnv

    /** Get files that are a library (i.e. that do not run anything) */
    protected def getLibJSFiles(): Seq[VirtualJSFile] =
      initFiles() ++ customInitFiles() ++ libs.map(_.lib)

    /** Get all files that are passed to VM (libraries and code) */
    protected def getJSFiles(): Seq[VirtualJSFile] =
      getLibJSFiles() :+ code

    /** write a single JS file to a writer using an include fct if appropriate */
    protected def writeJSFile(file: VirtualJSFile, writer: Writer): Unit = {
      // The only platform-independent way to do this in JS is to dump the file.
      writer.write(file.content)
      writer.write('\n')
    }

    /** Pipe stdin and stdout from/to VM */
    final protected def pipeVMData(vmInst: Process): Unit = {
      // Send stdin to VM.
      val out = vmInst.getOutputStream()
      try { sendVMStdin(out) }
      finally { out.close() }

      // Pipe stdout to console
      pipeToConsole(vmInst.getInputStream(), console)

      // We are probably done (stdin is closed). Report any errors
      val errSrc = Source.fromInputStream(vmInst.getErrorStream(), "UTF-8")
      try { errSrc.getLines.foreach(err => logger.error(err)) }
      finally { errSrc.close }
    }

    /** Wait for the VM to terminate, verify exit code
     *
     *  @throws ExternalJSEnv.NonZeroExitException if VM returned a non-zero code
     */
    final protected def waitForVM(vmInst: Process): Unit = {
      // Make sure we are done.
      vmInst.waitFor()

      // Get return value and return
      val retVal = vmInst.exitValue
      if (retVal != 0)
        throw new NonZeroExitException(vmName, retVal)
    }

    protected def startVM(): Process = {
      val vmArgs = getVMArgs()
      val vmEnv  = getVMEnv()

      val allArgs = executable +: vmArgs
      val pBuilder = new ProcessBuilder(allArgs: _*)

      pBuilder.environment().clear()
      for ((name, value) <- vmEnv)
        pBuilder.environment().put(name, value)

      logger.debug("Starting process: " + allArgs.mkString(" "))

      pBuilder.start()
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

    /** pipe lines from input stream to JSConsole */
    final protected def pipeToConsole(in: InputStream, console: JSConsole) = {
      val source = Source.fromInputStream(in, "UTF-8")
      try { source.getLines.foreach(console.log _) }
      finally { source.close() }
    }

  }

  protected class ExtRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile)
      extends AbstractExtRunner(libs, code) with JSRunner {

    def run(logger: Logger, console: JSConsole): Unit = {
      setupLoggerAndConsole(logger, console)

      val vmInst = startVM()

      pipeVMData(vmInst)
      waitForVM(vmInst)
    }
  }

  protected class AsyncExtRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile)
      extends AbstractExtRunner(libs, code) with AsyncJSRunner {

    private[this] var vmInst: Process = null
    private[this] var ioThreadEx: Throwable = null
    private[this] val promise = Promise[Unit]

    private[this] val thread = new Thread {
      override def run(): Unit = {
        // This thread should not be interrupted, so it is safe to use Trys
        val pipeResult = Try(pipeVMData(vmInst))
        val vmComplete = Try(waitForVM(vmInst))

        // Store IO exception
        pipeResult recover {
          case e => ioThreadEx = e
        }

        // Chain Try's the other way: We want VM failure first, then IO failure
        promise.complete(pipeResult orElse vmComplete)
      }
    }

    def future: Future[Unit] = promise.future

    def start(logger: Logger, console: JSConsole): Future[Unit] = {
      setupLoggerAndConsole(logger, console)
      startExternalJSEnv()
      future
    }

    /** Core functionality of [[start]].
     *
     *  Same as [[start]] but without a call to [[setupLoggerAndConsole]] and
     *  not returning [[future]].
     *  Useful to be called in overrides of [[start]].
     */
    protected def startExternalJSEnv(): Unit = {
      require(vmInst == null, "start() may only be called once")
      vmInst = startVM()
      thread.start()
    }

    def stop(): Unit = {
      require(vmInst != null, "start() must have been called")
      vmInst.destroy()
    }
  }

}

object ExternalJSEnv {
  final case class NonZeroExitException(vmName: String, retVal: Int)
      extends Exception(s"$vmName exited with code $retVal")
}
