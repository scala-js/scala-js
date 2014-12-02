/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.rhino

import org.scalajs.jsenv._

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.logging._

import scala.io.Source

import scala.collection.mutable

import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration.Duration

import org.mozilla.javascript._

final class RhinoJSEnv private (
    semantics: Semantics,
    withDOM: Boolean,
    sourceMap: Boolean
) extends ComJSEnv {

  import RhinoJSEnv._

  def this(semantics: Semantics = Semantics.Defaults, withDOM: Boolean = false) =
    this(semantics, withDOM, sourceMap = true)

  def withSourceMap(sourceMap: Boolean): RhinoJSEnv =
    new RhinoJSEnv(semantics, withDOM, sourceMap)

  /** Executes code in an environment where the Scala.js library is set up to
   *  load its classes lazily.
   *
   *  Other .js scripts in the inputs are executed eagerly before the provided
   *  `code` is called.
   */
  override def jsRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): JSRunner = {
    new Runner(classpath, code, logger, console)
  }

  private class Runner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole) extends JSRunner {
    def run(): Unit = internalRunJS(classpath, code, logger, console, None)
  }

  override def asyncRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): AsyncJSRunner = {
    new AsyncRunner(classpath, code, logger, console)
  }

  private class AsyncRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole) extends AsyncJSRunner {

    private[this] val promise = Promise[Unit]

    private[this] val thread = new Thread {
      override def run(): Unit = {
        try {
          internalRunJS(classpath, code, logger, console, optChannel)
          promise.success(())
        } catch {
          case t: Throwable =>
            promise.failure(t)
        }
      }
    }

    def future: Future[Unit] = promise.future

    def start(): Future[Unit] = {
      thread.start()
      future
    }

    def stop(): Unit = thread.interrupt()

    protected def optChannel(): Option[Channel] = None
  }

  override def comRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): ComJSRunner = {
    new ComRunner(classpath, code, logger, console)
  }

  private class ComRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole)
      extends AsyncRunner(classpath, code, logger, console) with ComJSRunner {

    private[this] val channel = new Channel

    override protected def optChannel(): Option[Channel] = Some(channel)

    def send(msg: String): Unit = channel.sendToJS(msg)

    def receive(): String = {
      try {
        channel.recvJVM()
      } catch {
        case _: ChannelClosedException =>
          throw new ComJSEnv.ComClosedException
      }
    }

    def close(): Unit = channel.closeJVM()

  }

  private def internalRunJS(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole, optChannel: Option[Channel]): Unit = {

    val context = Context.enter()
    try {
      val scope = context.initStandardObjects()

      if (withDOM)
        setupDOM(context, scope)

      disableLiveConnect(context, scope)
      setupConsole(context, scope, console)

      // Optionally setup scalaJSCom
      var recvCallback: Option[String => Unit] = None
      for (channel <- optChannel) {
        setupCom(context, scope, channel,
          setCallback = cb => recvCallback = Some(cb),
          clrCallback = () => recvCallback = None)
      }

      try {
        loadClasspath(context, scope, classpath)

        // Actually run the code
        context.evaluateFile(scope, code)

        // Callback the com channel if necessary
        // (if recvCallback = None, channel wasn't initialized on the client)
        for ((channel, callback) <- optChannel zip recvCallback)
          msgReceiveLoop(channel, callback, () => recvCallback.isDefined)

      } catch {
        case e: RhinoException =>
          // Trace here, since we want to be in the context to trace.
          logger.trace(e)
          sys.error(s"Exception while running JS code: ${e.getMessage}")
      }
    } finally {
      // Ensure the channel is closed to release JVM side
      optChannel.foreach(_.closeJS())

      Context.exit()
    }
  }

  private def setupDOM(context: Context, scope: Scriptable): Unit = {
    // Fetch env.rhino.js from webjar
    val name = "env.rhino.js"
    val path = "/META-INF/resources/webjars/envjs/1.2/" + name
    val resource = getClass.getResource(path)
    assert(resource != null, s"need $name as resource")

    // Rhino can't optimize envjs
    context.setOptimizationLevel(-1)

    // Don't print envjs header
    scope.addFunction("print", args => ())

    // Pipe file to Rhino
    val reader = Source.fromURL(resource).bufferedReader
    context.evaluateReader(scope, reader, name, 1, null);

    // No need to actually define print here: It is captured by envjs to
    // implement console.log, which we'll override in the next statement
  }

  /** Make sure Rhino does not do its magic for JVM top-level packages (#364) */
  private def disableLiveConnect(context: Context, scope: Scriptable): Unit = {
    val PackagesObject =
      ScriptableObject.getProperty(scope, "Packages").asInstanceOf[Scriptable]
    val topLevelPackageIds = ScriptableObject.getPropertyIds(PackagesObject)
    for (id <- topLevelPackageIds) (id: Any) match {
      case name: String => ScriptableObject.deleteProperty(scope, name)
      case index: Int   => ScriptableObject.deleteProperty(scope, index)
      case _            => // should not happen, I think, but with Rhino you never know
    }
  }

  private def setupConsole(context: Context, scope: Scriptable,
      console: JSConsole): Unit = {
    // Setup console.log
    val jsconsole = context.newObject(scope)
    jsconsole.addFunction("log", _.foreach(console.log _))
    ScriptableObject.putProperty(scope, "console", jsconsole)
  }

  private def setupCom(context: Context, scope: Scriptable, channel: Channel,
      setCallback: (String => Unit) => Unit, clrCallback: () => Unit): Unit = {

    val comObj = context.newObject(scope)

    comObj.addFunction("send", s =>
      channel.sendToJVM(Context.toString(s(0))))

    comObj.addFunction("init", s => s(0) match {
      case f: Function =>
        val cb: String => Unit =
          msg => f.call(context, scope, scope, Array(msg))
        setCallback(cb)
      case _ =>
        sys.error("First argument to init must be a function")
    })

    comObj.addFunction("close", _ => {
      // Tell JVM side we won't send anything
      channel.closeJS()
      // Internally register that we're done
      clrCallback()
    })

    ScriptableObject.putProperty(scope, "scalajsCom", comObj)
  }

  /** Loads the classpath. Either through lazy loading or by simply inserting */
  private def loadClasspath(context: Context, scope: Scriptable,
      classpath: CompleteClasspath): Unit = classpath match {
    case cp: IRClasspath =>
      // Setup lazy loading classpath and source mapper
      val optLoader = if (cp.scalaJSIR.nonEmpty) {
        val loader = new ScalaJSCoreLib(semantics, cp)

        // Setup sourceMapper
        if (sourceMap) {
          val scalaJSenv = context.newObject(scope)

          scalaJSenv.addFunction("sourceMapper", args => {
            val trace = Context.toObject(args(0), scope)
            loader.mapStackTrace(trace, context, scope)
          })

          ScriptableObject.putProperty(scope, "__ScalaJSEnv", scalaJSenv)
        }

        Some(loader)
      } else {
        None
      }

      // Load JS libraries
      cp.jsLibs.foreach(dep => context.evaluateFile(scope, dep.lib))

      optLoader.foreach(_.insertInto(context, scope))

    case cp =>
      cp.allCode.foreach(context.evaluateFile(scope, _))
  }

  private def msgReceiveLoop(channel: Channel,
      callback: String => Unit, stillOpen: () => Boolean): Unit = {

    try {
      while (stillOpen())
        callback(channel.recvJS())
    } catch {
      case _: ChannelClosedException =>
        // the JVM side closed the connection
    }
  }

}

object RhinoJSEnv {

  /** Communication channel between the Rhino thread and the rest of the JVM */
  private class Channel {
    private[this] var _closedJS = false
    private[this] var _closedJVM = false
    private[this] val js2jvm = mutable.Queue.empty[String]
    private[this] val jvm2js = mutable.Queue.empty[String]

    def sendToJS(msg: String): Unit = synchronized {
      ensureOpen(_closedJVM)
      jvm2js.enqueue(msg)
      notify()
    }

    def sendToJVM(msg: String): Unit = synchronized {
      ensureOpen(_closedJS)
      js2jvm.enqueue(msg)
      notify()
    }

    def recvJVM(): String = synchronized {
      while (js2jvm.isEmpty && ensureOpen(_closedJS))
        wait()

      js2jvm.dequeue()
    }

    def recvJS(): String = synchronized {
      while (jvm2js.isEmpty && ensureOpen(_closedJVM))
        wait()

      jvm2js.dequeue()
    }

    def closeJS(): Unit = synchronized {
      _closedJS = true
      notify()
    }

    def closeJVM(): Unit = synchronized {
      _closedJVM = true
      notify()
    }

    /** Throws if the channel is closed and returns true */
    private def ensureOpen(closed: Boolean): Boolean = {
      if (closed)
        throw new ChannelClosedException
      true
    }
  }

  private class ChannelClosedException extends Exception

}
