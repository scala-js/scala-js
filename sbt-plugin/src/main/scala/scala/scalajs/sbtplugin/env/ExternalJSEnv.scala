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
  final protected val additionalEnv:  Map[String, String]) extends AsyncJSEnv {

  /** Printable name of this VM */
  protected def vmName: String

  /** Command to execute (on shell) for this VM */
  protected def executable: String

  protected class AbstractExtRunner(protected val classpath: CompleteClasspath,
      protected val code: VirtualJSFile, protected val logger: Logger,
      protected val console: JSConsole) {

    /** JS files used to setup VM */
    protected def initFiles(): Seq[VirtualJSFile] = Nil

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
      initFiles() ++ classpath.allCode

    /** Get all files that are passed to VM (libraries and code) */
    protected def getJSFiles(): Seq[VirtualJSFile] =
      getLibJSFiles() :+ code

    /** write a single JS file to a writer using an include fct if appropriate */
    protected def writeJSFile(file: VirtualJSFile, writer: Writer): Unit = {
      file match {
        // TODO remove this case. It is VM specific
        case file: FileVirtualJSFile =>
          val fname = toJSstr(file.file.getAbsolutePath)
          writer.write(s"require($fname);\n")
        case _ =>
          writer.write(file.content)
          writer.write('\n')
      }
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

    /** Wait for the VM to terminate, verify exit code */
    final protected def waitForVM(vmInst: Process): Unit = {
      // Make sure we are done.
      vmInst.waitFor()

      // Get return value and return
      val retVal = vmInst.exitValue
      if (retVal != 0)
        sys.error(s"$vmName exited with code $retVal")
    }

    protected def startVM(): Process = {
      val vmArgs = getVMArgs()
      val vmEnv  = getVMEnv()

      val allArgs = executable +: vmArgs
      val pBuilder = new ProcessBuilder(allArgs: _*)

      pBuilder.environment().clear()
      for ((name, value) <- vmEnv)
        pBuilder.environment().put(name, value)

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

  protected class ExtRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole
  ) extends AbstractExtRunner(classpath, code, logger, console)
       with JSRunner {

    def run(): Unit = {
      val vmInst = startVM()

      pipeVMData(vmInst)
      waitForVM(vmInst)
    }
  }

  protected class AsyncExtRunner(classpath: CompleteClasspath,
      code: VirtualJSFile, logger: Logger, console: JSConsole
  ) extends AbstractExtRunner(classpath, code, logger, console)
       with AsyncJSRunner {

    private[this] var vmInst: Process = null
    private[this] var ioThreadEx: Throwable = null

    private[this] val thread = new Thread {
      override def run(): Unit = {
        try {
          pipeVMData(vmInst)
        } catch {
          case e: Throwable => ioThreadEx = e
        }
      }
    }

    def start(): Unit = {
      require(vmInst == null, "start() may only be called once")
      vmInst = startVM()
      thread.start()
    }

    def stop(): Unit = {
      require(vmInst != null, "start() must have been called")
      vmInst.destroy()
    }

    def isRunning(): Boolean = {
      require(vmInst != null, "start() must have been called")
      // Emulate JDK 8 Process.isAlive
      try {
        vmInst.exitValue()
        false
      } catch {
        case e: IllegalThreadStateException =>
          true
      }
    }

    def await(): Unit = {
      require(vmInst != null, "start() must have been called")
      thread.join()
      waitForVM(vmInst)

      // At this point, the VM itself didn't fail. We need to check if
      // anything bad happened while piping the data from the VM

      if (ioThreadEx != null)
        throw ioThreadEx
    }
  }

}
