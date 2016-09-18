/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.rhino

import org.scalajs.jsenv._
import org.scalajs.jsenv.Utils.OptDeadline

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.core.tools.logging._

import org.scalajs.core.tools.linker.LinkingUnit
import org.scalajs.core.tools.linker.backend.OutputMode
import org.scalajs.core.tools.linker.backend.emitter.Emitter
import org.scalajs.core.tools.javascript.ESLevel

import scala.annotation.tailrec

import scala.io.Source

import scala.collection.mutable

import scala.concurrent.{Future, Promise, Await, TimeoutException}
import scala.concurrent.duration._

import scala.reflect.ClassTag

import org.mozilla.javascript._

/** A JS environment using a modified Rhino interpreter (deprecated).
 *
 *  As of Scala.js 0.6.13, `RhinoJSEnv` is deprecated. It will be removed in
 *  Scala.js 1.0.0.
 */
final class RhinoJSEnv private (
    semantics: Semantics,
    withDOM: Boolean,
    val sourceMap: Boolean
) extends LinkingUnitComJSEnv {

  import RhinoJSEnv._

  @deprecated(
      "The Rhino JS environment is being phased out. " +
      "It will be removed in Scala.js 1.0.0. ",
      "0.6.13")
  def this(semantics: Semantics = Semantics.Defaults, withDOM: Boolean = false) =
    this(semantics, withDOM, sourceMap = true)

  /** A non-deprecated constructor for internal use. */
  private[scalajs] def this(semantics: Semantics, withDOM: Boolean,
      internal: Unit) = {
    this(semantics, withDOM, sourceMap = true)
  }

  def withSourceMap(sourceMap: Boolean): RhinoJSEnv =
    new RhinoJSEnv(semantics, withDOM, sourceMap)

  /* Ask the Emitter, which we'll use in ScalaJSCoreLib to generate JS code,
   * what are its requirements.
   */
  val symbolRequirements = Emitter.symbolRequirements(semantics, ESLevel.ES5)

  def name: String = "RhinoJSEnv"

  override def loadLinkingUnit(linkingUnit: LinkingUnit): ComJSEnv = {
    verifyUnit(linkingUnit)
    super.loadLinkingUnit(linkingUnit)
  }

  /** Executes code in an environment where the Scala.js library is set up to
   *  load its classes lazily.
   *
   *  Other .js scripts in the inputs are executed eagerly before the provided
   *  `code` is called.
   */
  override def jsRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): JSRunner = {
    new Runner(libs, None, Nil, code)
  }

  override def jsRunner(preLibs: Seq[ResolvedJSDependency],
      linkingUnit: LinkingUnit, postLibs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): JSRunner = {
    verifyUnit(linkingUnit)
    new Runner(preLibs, Some(linkingUnit), postLibs, code)
  }

  private class Runner(preLibs: Seq[ResolvedJSDependency],
      optLinkingUnit: Option[LinkingUnit], postLibs: Seq[ResolvedJSDependency],
      code: VirtualJSFile) extends JSRunner {
    def run(logger: Logger, console: JSConsole): Unit =
      internalRunJS(preLibs, optLinkingUnit, postLibs,
          code, logger, console, None)
  }

  override def asyncRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): AsyncJSRunner = {
    new AsyncRunner(libs, None, Nil, code)
  }

  override def asyncRunner(preLibs: Seq[ResolvedJSDependency],
      linkingUnit: LinkingUnit, postLibs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): AsyncJSRunner = {
    verifyUnit(linkingUnit)
    new AsyncRunner(preLibs, Some(linkingUnit), postLibs, code)
  }

  private class AsyncRunner(preLibs: Seq[ResolvedJSDependency],
      optLinkingUnit: Option[LinkingUnit], postLibs: Seq[ResolvedJSDependency],
      code: VirtualJSFile) extends AsyncJSRunner {

    private[this] val promise = Promise[Unit]
    private[this] var _thread: Thread = _

    def future: Future[Unit] = promise.future

    def start(logger: Logger, console: JSConsole): Future[Unit] = {
      _thread = new Thread {
        override def run(): Unit = {
          try {
            internalRunJS(preLibs, optLinkingUnit, postLibs,
                code, logger, console, optChannel)
            promise.success(())
          } catch {
            case t: Throwable =>
              promise.failure(t)
          }
        }
      }

      _thread.start()
      future
    }

    def stop(): Unit = _thread.interrupt()

    protected def optChannel(): Option[Channel] = None
  }

  override def comRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): ComJSRunner = {
    new ComRunner(libs, None, Nil, code)
  }

  override def comRunner(preLibs: Seq[ResolvedJSDependency],
      linkingUnit: LinkingUnit, postLibs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): ComJSRunner = {
    verifyUnit(linkingUnit)
    new ComRunner(preLibs, Some(linkingUnit), postLibs, code)
  }

  private class ComRunner(preLibs: Seq[ResolvedJSDependency],
      optLinkingUnit: Option[LinkingUnit], postLibs: Seq[ResolvedJSDependency],
      code: VirtualJSFile)
      extends AsyncRunner(preLibs, optLinkingUnit, postLibs, code)
      with ComJSRunner {

    private[this] val channel = new Channel

    override protected def optChannel(): Option[Channel] = Some(channel)

    def send(msg: String): Unit = channel.sendToJS(msg)

    def receive(timeout: Duration): String = {
      try {
        channel.recvJVM(timeout)
      } catch {
        case _: ChannelClosedException =>
          throw new ComJSEnv.ComClosedException
      }
    }

    def close(): Unit = channel.closeJVM()

  }

  private def internalRunJS(preLibs: Seq[ResolvedJSDependency],
      optLinkingUnit: Option[LinkingUnit], postLibs: Seq[ResolvedJSDependency],
      code: VirtualJSFile, logger: Logger, console: JSConsole,
      optChannel: Option[Channel]): Unit = {

    val context = Context.enter()
    try {
      val scope = context.initStandardObjects()

      // Rhino has trouble optimizing some big things, e.g., env.js or ScalaTest
      context.setOptimizationLevel(-1)

      if (withDOM)
        setupDOM(context, scope)

      disableLiveConnect(context, scope)
      setupConsole(context, scope, console)

      val taskQ = setupSetTimeout(context, scope)

      // Optionally setup scalaJSCom
      var recvCallback: Option[String => Unit] = None
      for (channel <- optChannel) {
        setupCom(context, scope, channel,
          setCallback = cb => recvCallback = Some(cb),
          clrCallback = () => recvCallback = None)
      }

      try {
        // Evaluate pre JS libs
        preLibs.foreach(lib => context.evaluateFile(scope, lib.lib))

        // Load LinkingUnit (if present)
        optLinkingUnit.foreach(loadLinkingUnit(context, scope, _))

        // Evaluate post JS libs
        postLibs.foreach(lib => context.evaluateFile(scope, lib.lib))

        // Actually run the code
        context.evaluateFile(scope, code)

        // Start the event loop

        for (channel <- optChannel) {
          comEventLoop(taskQ, channel,
              () => recvCallback.get, () => recvCallback.isDefined)
        }

        // Channel is closed. Fall back to basic event loop
        basicEventLoop(taskQ)

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

  private def setupSetTimeout(context: Context,
      scope: Scriptable): TaskQueue = {

    val ordering = Ordering.by[TimedTask, Deadline](_.deadline).reverse
    val taskQ = mutable.PriorityQueue.empty(ordering)

    def ensure[T: ClassTag](v: AnyRef, errMsg: String) = v match {
      case v: T => v
      case _    => sys.error(errMsg)
    }

    scope.addFunction("setTimeout", args => {
      val cb = ensure[Function](args(0),
          "First argument to setTimeout must be a function")

      val deadline =
        args.lift(1).fold(0)(n => Context.toNumber(n).toInt).millis.fromNow

      val task = new TimeoutTask(deadline, () =>
        cb.call(context, scope, scope, args.slice(2, args.length)))

      taskQ += task

      task
    })

    scope.addFunction("setInterval", args => {
      val cb = ensure[Function](args(0),
          "First argument to setInterval must be a function")

      val interval = Context.toNumber(args(1)).toInt.millis
      val firstDeadline = interval.fromNow

      val task = new IntervalTask(firstDeadline, interval, () =>
        cb.call(context, scope, scope, args.slice(2, args.length)))

      taskQ += task

      task
    })

    scope.addFunction("clearTimeout", args => {
      val task = ensure[TimeoutTask](args(0), "First argument to " +
          "clearTimeout must be a value returned by setTimeout")
      task.cancel()
    })

    scope.addFunction("clearInterval", args => {
      val task = ensure[IntervalTask](args(0), "First argument to " +
          "clearInterval must be a value returned by setInterval")
      task.cancel()
    })

    taskQ
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

  /** Loads a [[LinkingUnit]] with lazy loading of classes and source mapping. */
  private def loadLinkingUnit(context: Context, scope: Scriptable,
      linkingUnit: LinkingUnit): Unit = {

    val loader = new ScalaJSCoreLib(linkingUnit)

    // Setup sourceMapper
    if (sourceMap) {
      val oldScalaJSenv = ScriptableObject.getProperty(scope, "__ScalaJSEnv")
      val scalaJSenv = oldScalaJSenv match {
        case Scriptable.NOT_FOUND =>
          val newScalaJSenv = context.newObject(scope)
          ScriptableObject.putProperty(scope, "__ScalaJSEnv", newScalaJSenv)
          newScalaJSenv

        case oldScalaJSenv: Scriptable =>
          oldScalaJSenv
      }

      scalaJSenv.addFunction("sourceMapper", args => {
        val trace = Context.toObject(args(0), scope)
        loader.mapStackTrace(trace, context, scope)
      })
    }

    loader.insertInto(context, scope)
  }

  private def basicEventLoop(taskQ: TaskQueue): Unit =
    eventLoopImpl(taskQ, sleepWait, () => true)

  private def comEventLoop(taskQ: TaskQueue, channel: Channel,
      callback: () => String => Unit, isOpen: () => Boolean): Unit = {

    if (!isOpen())
      // The channel has not been opened yet. Wait for opening.
      eventLoopImpl(taskQ, sleepWait, () => !isOpen())

    // Once we reach this point, we either:
    // - Are done
    // - The channel is open

    // Guard call to `callback`
    if (isOpen()) {
      val cb = callback()
      try {
        @tailrec
        def loop(): Unit = {
          val loopResult = eventLoopImpl(taskQ, channel.recvJS _, isOpen)

          loopResult match {
            case Some(msg) =>
              cb(msg)
              loop()
            case None if isOpen() =>
              assert(taskQ.isEmpty)
              cb(channel.recvJS())
              loop()
            case None =>
              // No tasks left, channel closed
          }
        }
        loop()
      } catch {
        case _: ChannelClosedException =>
          // the JVM side closed the connection
      }
    }
  }

  /** Run an event loop on [[taskQ]] using [[waitFct]] to wait
   *
   *  If [[waitFct]] returns a Some, this method returns this value immediately
   *  If [[waitFct]] returns a None, we assume a sufficient amount has been
   *  waited for the Deadline to pass. The event loop then runs the task.
   *
   *  Each iteration, [[continue]] is queried, whether to continue the loop.
   *
   *  @returns A Some returned by [[waitFct]] or None if [[continue]] has
   *      returned false, or there are no more tasks (i.e. [[taskQ]] is empty)
   *  @throws InterruptedException if the thread was interrupted
   */
  private def eventLoopImpl[T](taskQ: TaskQueue,
      waitFct: Deadline => Option[T], continue: () => Boolean): Option[T] = {

    @tailrec
    def loop(): Option[T] = {
      if (Thread.interrupted())
        throw new InterruptedException()

      if (taskQ.isEmpty || !continue()) None
      else {
        val task = taskQ.head
        if (task.canceled) {
          taskQ.dequeue()
          loop()
        } else {
          waitFct(task.deadline) match {
            case result @ Some(_) => result

            case None =>
              // The time has actually expired
              val task = taskQ.dequeue()

              // Perform task
              task.task()

              if (task.reschedule())
                taskQ += task

              loop()
          }
        }
      }
    }

    loop()
  }

  private val sleepWait = { (deadline: Deadline) =>
    val timeLeft = deadline.timeLeft.toMillis
    if (timeLeft > 0)
      Thread.sleep(timeLeft)
    None
  }

  private def verifyUnit(linkingUnit: LinkingUnit) = {
    require(linkingUnit.semantics == semantics,
        "RhinoJSEnv and LinkingUnit must agree on semantics")
    require(linkingUnit.esLevel == ESLevel.ES5, "RhinoJSEnv only supports ES5")
  }

}

object RhinoJSEnv {

  final class ClassNotFoundException(className: String) extends Exception(
    s"Rhino was unable to load Scala.js class: $className")

  /** Communication channel between the Rhino thread and the rest of the JVM */
  private class Channel {
    private[this] var _closedJS = false
    private[this] var _closedJVM = false
    private[this] val js2jvm = mutable.Queue.empty[String]
    private[this] val jvm2js = mutable.Queue.empty[String]

    def sendToJS(msg: String): Unit = synchronized {
      ensureOpen(_closedJVM)
      jvm2js.enqueue(msg)
      notifyAll()
    }

    def sendToJVM(msg: String): Unit = synchronized {
      ensureOpen(_closedJS)
      js2jvm.enqueue(msg)
      notifyAll()
    }

    def recvJVM(timeout: Duration): String = synchronized {
      val deadline = OptDeadline(timeout)

      while (js2jvm.isEmpty && ensureOpen(_closedJS) && !deadline.isOverdue)
        wait(deadline.millisLeft)

      if (js2jvm.isEmpty)
        throw new TimeoutException("Timeout expired")
      js2jvm.dequeue()
    }

    def recvJS(): String = synchronized {
      while (jvm2js.isEmpty && ensureOpen(_closedJVM))
        wait()

      jvm2js.dequeue()
    }

    def recvJS(deadline: Deadline): Option[String] = synchronized {
      var expired = false
      while (jvm2js.isEmpty && !expired && ensureOpen(_closedJVM)) {
        val timeLeft = deadline.timeLeft.toMillis
        if (timeLeft > 0)
          wait(timeLeft)
        else
          expired = true
      }

      if (expired) None
      else Some(jvm2js.dequeue())
    }

    def closeJS(): Unit = synchronized {
      _closedJS = true
      notifyAll()
    }

    def closeJVM(): Unit = synchronized {
      _closedJVM = true
      notifyAll()
    }

    /** Throws if the channel is closed and returns true */
    private def ensureOpen(closed: Boolean): Boolean = {
      if (closed)
        throw new ChannelClosedException
      true
    }
  }

  private class ChannelClosedException extends Exception

  private abstract class TimedTask(val task: () => Unit) {
    private[this] var _canceled: Boolean = false

    def deadline: Deadline
    def reschedule(): Boolean

    def canceled: Boolean = _canceled
    def cancel(): Unit = _canceled = true
  }

  private final class TimeoutTask(val deadline: Deadline,
      task: () => Unit) extends TimedTask(task) {
    def reschedule(): Boolean = false

    override def toString(): String =
      s"TimeoutTask($deadline, canceled = $canceled)"
  }

  private final class IntervalTask(firstDeadline: Deadline,
      interval: FiniteDuration, task: () => Unit) extends TimedTask(task) {

    private[this] var _deadline = firstDeadline

    def deadline: Deadline = _deadline

    def reschedule(): Boolean = {
      _deadline += interval
      !canceled
    }

    override def toString(): String =
      s"IntervalTask($deadline, interval = $interval, canceled = $canceled)"
  }

  private type TaskQueue = mutable.PriorityQueue[TimedTask]

}
