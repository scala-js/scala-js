/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js JS Envs           **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2017, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv

import java.io.{IOException, OutputStream}

import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/** Support for creating a [[JSRun]] via an external process. */
object ExternalJSRun {
  /** Starts a [[JSRun]] in an external process.
   *
   *  [[ExternalJSRun]] redirects the I/O of the external process according to
   *  [[Config#runConfig]].
   *
   *  @see [[supports]] for the exact options it currently supports.
   *
   *  @param command Binary to execute including arguments.
   *  @param config Configuration.
   *  @param input Function to inform about creation of stdin for the external process.
   *      `input` should feed the required stdin to the passed
   *      [[java.io.OutputStream OutputStream]] and close it.
   */
  def start(command: List[String], config: Config)(
      input: OutputStream => Unit): JSRun = {
    require(command.nonEmpty, "command may not be empty")

    try {
      val process = startProcess(command, config.env, config.runConfig)
      try {
        notifyOutputStreams(config.runConfig, process)

        new ExternalJSRun(process, input, config.closingFails)
      } catch {
        case t: Throwable =>
          process.destroyForcibly()
          throw t
      }
    } catch {
      case NonFatal(t) => JSRun.failed(t)
    }
  }

  /** Informs the given [[RunConfig.Validator]] about the options an
   *  [[ExternalJSRun]] supports.
   *
   *  Use this method to automatically benefit from improvements to
   *  [[ExternalJSRun]] without modifying the client [[JSEnv]].
   *
   *  Currently, this calls
   *  - [[RunConfig.Validator#supportsInheritIO supportsInheritIO]]
   *  - [[RunConfig.Validator#supportsOnOutputStream supportsOnOutputStream]]
   *
   *  Note that in consequence, a [[JSEnv]] ''may not'' handle these options if
   *  it uses [[ExternalJSRun]].
   */
  def supports(validator: RunConfig.Validator): RunConfig.Validator = {
    validator
      .supportsInheritIO()
      .supportsOnOutputStream()
  }

  /** Configuration for a [[ExternalJSRun]]
   *
   *  @param env Additional environment variables. The environment of the host
   *      JVM is inherited.
   *  @param runConfig Configuration for the run. See [[ExternalJSRun.supports]]
   *      for details about the currently supported configuration.
   *  @param closingFails Whether calling [[JSRun#close]] on a still running
   *      [[JSRun]] fails the run. While this defaults to true, [[JSEnv]]s that
   *      do not support automatic termination (and do not expect the JS program
   *      itself to explicitly terminate) typically want to set this to false
   *      (at least for non-com runs), since otherwise there is no successful
   *      way of terminating a [[JSRun]].
   */
  final class Config private (
      val env: Map[String, String],
      val runConfig: RunConfig,
      val closingFails: Boolean
  ) {
    private def this() = {
      this(
          env = Map.empty,
          runConfig = RunConfig(),
          closingFails = true)
    }

    def withEnv(env: Map[String, String]): Config =
      copy(env = env)

    def withRunConfig(runConfig: RunConfig): Config =
      copy(runConfig = runConfig)

    def withClosingFails(closingFails: Boolean): Config =
      copy(closingFails = closingFails)

    private def copy(env: Map[String, String] = env,
        runConfig: RunConfig = runConfig,
        closingFails: Boolean = closingFails) = {
      new Config(env, runConfig, closingFails)
    }
  }

  object Config {
    def apply(): Config = new Config()
  }

  private def notifyOutputStreams(config: RunConfig, process: Process) = {
    def opt[T](b: Boolean, v: => T) = if (b) Some(v) else None

    val out = opt(!config.inheritOutput, process.getInputStream())
    val err = opt(!config.inheritError, process.getErrorStream())

    config.onOutputStream.foreach(f => f(out, err))
  }

  private def startProcess(command: List[String], env: Map[String, String],
      config: RunConfig) = {
    val builder = new ProcessBuilder(command: _*)

    if (config.inheritOutput)
      builder.redirectOutput(ProcessBuilder.Redirect.INHERIT)

    if (config.inheritError)
      builder.redirectError(ProcessBuilder.Redirect.INHERIT)

    for ((name, value) <- env)
      builder.environment().put(name, value)

    config.logger.debug("Starting process: " + command.mkString(" "))

    builder.start()
  }

  final case class NonZeroExitException(retVal: Int)
      extends Exception(s"exited with code $retVal")

  final case class ClosedException()
      extends Exception("Termination was requested by user")
}

private final class ExternalJSRun(process: Process,
    input: OutputStream => Unit, closingFails: Boolean) extends JSRun {

  private[this] val promise = Promise[Unit]()

  @volatile
  private[this] var closing = false

  def future: Future[Unit] = promise.future

  def close(): Unit = {
    closing = true
    process.destroyForcibly()
  }

  private val waiter = new Thread {
    setName("ExternalJSRun waiter")

    override def run(): Unit = {
      try {
        try {
          input(process.getOutputStream())
        } catch {
          case _: IOException if closing =>
            // We got closed while writing. Exception is expected.
        }

        val retVal = process.waitFor()
        if (retVal == 0 || closing && !closingFails)
          promise.success(())
        else if (closing)
          promise.failure(new ExternalJSRun.ClosedException)
        else
          promise.failure(new ExternalJSRun.NonZeroExitException(retVal))
      } catch {
        case t: Throwable =>
          process.destroyForcibly()
          promise.failure(t)

          if (!NonFatal(t))
            throw t
      }
    }
  }

  waiter.start()
}
