/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.env.rhino

import scala.scalajs.tools.sem.Semantics
import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.env._
import scala.scalajs.tools.logging._

import scala.io.Source

import scala.collection.mutable

import org.mozilla.javascript._

class RhinoJSEnv(semantics: Semantics,
    withDOM: Boolean = false) extends ComJSEnv {

  import RhinoJSEnv._

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

    private[this] var resultThrowable: Throwable = null

    private[this] val thread = new Thread {
      override def run(): Unit = {
        try {
          internalRunJS(classpath, code, logger, console, optChannel)
        } catch {
          case t: Throwable => resultThrowable = t
        }
      }
    }

    def start(): Unit = thread.start()

    def isRunning(): Boolean = thread.isAlive()

    def await(): Unit = {
      thread.join()
      if (resultThrowable != null)
        throw resultThrowable
    }

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

    def send(msg: String): Unit = {
      try {
        channel.sendToJS(msg)
      } catch {
        case _: ChannelClosedException =>
          throw new ComJSEnv.ComClosedException
      }
    }

    def receive(): String = {
      try {
        channel.recvJVM()
      } catch {
        case _: ChannelClosedException =>
          throw new ComJSEnv.ComClosedException
      }
    }

    def close(): Unit = channel.close()

  }

  private def internalRunJS(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole, optChannel: Option[Channel]): Unit = {

    val context = Context.enter()
    try {
      val scope = context.initStandardObjects()

      if (withDOM) {
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

      // Make sure Rhino does not do its magic for JVM top-level packages (#364)
      val PackagesObject =
        ScriptableObject.getProperty(scope, "Packages").asInstanceOf[Scriptable]
      val topLevelPackageIds = ScriptableObject.getPropertyIds(PackagesObject)
      for (id <- topLevelPackageIds) (id: Any) match {
        case name: String => ScriptableObject.deleteProperty(scope, name)
        case index: Int   => ScriptableObject.deleteProperty(scope, index)
        case _            => // should not happen, I think, but with Rhino you never know
      }

      // Setup console.log
      val jsconsole = context.newObject(scope)
      jsconsole.addFunction("log", _.foreach(console.log _))
      ScriptableObject.putProperty(scope, "console", jsconsole)

      // Optionally setup scalaJSCom
      var recvCallback: Option[String => Unit] = None
      for (channel <- optChannel) {
        val comObj = context.newObject(scope)

        comObj.addFunction("send", s =>
          channel.sendToJVM(Context.toString(s(0))))

        comObj.addFunction("init", s => s(0) match {
          case f: Function =>
            val cb: String => Unit =
              msg => f.call(context, scope, scope, Array(msg))
            recvCallback = Some(cb)
          case _ =>
            sys.error("First argument to init must be a function")
        })

        comObj.addFunction("close", _ => {
          // Tell JVM side we won't send anything
          channel.close()
          // Internally register that we're done
          recvCallback = None
        })

        ScriptableObject.putProperty(scope, "scalajsCom", comObj)
      }

      try {
        // Make the classpath available. Either through lazy loading or by
        // simply inserting
        classpath match {
          case cp: IRClasspath =>
            // Setup lazy loading classpath and source mapper
            val optLoader = if (cp.scalaJSIR.nonEmpty) {
              val loader = new ScalaJSCoreLib(semantics, cp)

              // Setup sourceMapper
              val scalaJSenv = context.newObject(scope)

              scalaJSenv.addFunction("sourceMapper", args => {
                val trace = Context.toObject(args(0), scope)
                loader.mapStackTrace(trace, context, scope)
              })

              ScriptableObject.putProperty(scope, "__ScalaJSEnv", scalaJSenv)

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

        context.evaluateFile(scope, code)

        // Callback the com channel if necessary (if comCallback = None, channel
        // wasn't initialized on the client)
        for ((channel, callback) <- optChannel zip recvCallback) {
          try {
            while (recvCallback.isDefined)
              callback(channel.recvJS())
          } catch {
            case _: ChannelClosedException =>
              // the JVM side closed the connection
          }
        }

        // Enusre the channel is closed to release JVM side
        optChannel.foreach(_.close)

      } catch {
        case e: RhinoException =>
          // Trace here, since we want to be in the context to trace.
          logger.trace(e)
          sys.error(s"Exception while running JS code: ${e.getMessage}")
      }
    } finally {
      Context.exit()
    }
  }

}

object RhinoJSEnv {

  /** Communication channel between the Rhino thread and the rest of the JVM */
  private class Channel {
    private[this] var _closed = false
    private[this] val js2jvm = mutable.Queue.empty[String]
    private[this] val jvm2js = mutable.Queue.empty[String]

    def sendToJS(msg: String): Unit = synchronized {
      jvm2js.enqueue(msg)
      notify()
    }

    def sendToJVM(msg: String): Unit = synchronized {
      js2jvm.enqueue(msg)
      notify()
    }

    def recvJVM(): String = synchronized {
      while (js2jvm.isEmpty && ensureOpen())
        wait()

      js2jvm.dequeue()
    }

    def recvJS(): String = synchronized {
      while (jvm2js.isEmpty && ensureOpen())
        wait()

      jvm2js.dequeue()
    }

    def close(): Unit = synchronized {
      _closed = true
      notify()
    }

    /** Throws if the channel is closed and returns true */
    private def ensureOpen(): Boolean = {
      if (_closed)
        throw new ChannelClosedException
      true
    }
  }

  private class ChannelClosedException extends Exception

}
