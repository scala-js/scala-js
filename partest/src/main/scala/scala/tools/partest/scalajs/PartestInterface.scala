/* NOTE
 * Most of this file is copy-pasted from
 * https://github.com/scala/scala-partest-interface
 * It is unfortunately not configurable enough, hence the duplication
 */

package scala.tools.partest
package scalajs

import scala.language.reflectiveCalls

import _root_.sbt.testing._
import java.net.URLClassLoader
import java.io.File

object Framework {
  // as partest is not driven by test classes discovered by sbt, need to add this marker fingerprint to definedTests
  val fingerprint = new AnnotatedFingerprint { def isModule = true; def annotationName = "partest" }

  // TODO how can we export `fingerprint` so that a user can just add this to their build.sbt
  // definedTests in Test += new sbt.TestDefinition("partest", fingerprint, true, Array())
}
class Framework extends _root_.sbt.testing.Framework {
  def fingerprints: Array[Fingerprint] = Array(Framework.fingerprint)
  def name: String = "partest"

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): _root_.sbt.testing.Runner =
    new Runner(args, remoteArgs, testClassLoader)
}

/** Represents one run of a suite of tests.
 */
case class Runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader) extends _root_.sbt.testing.Runner {
  /** Returns an array of tasks that when executed will run tests and suites determined by the
   *  passed <code>TaskDef</code>s.
   *
   *  <p>
   *  Each returned task, when executed, will run tests and suites determined by the
   *  test class name, fingerprints, "explicitly specified" field, and selectors of one of the passed <code>TaskDef</code>s.
   *  </p>
   *
   *  <p>
   *  This <code>tasks</code> method may be called with <code>TaskDef</code>s containing the same value for <code>testClassName</code> but
   *  different fingerprints. For example, if both a class and its companion object were test classes, the <code>tasks</code> method could be
   *  passed an array containing <code>TaskDef</code>s with the same name but with a different value for <code>fingerprint.isModule</code>.
   *  </p>
   *
   *  <p>
   *  A test framework may "reject" a requested task by returning no <code>Task</code> for that <code>TaskDef</code>.
   *  </p>
   *
   *  @param taskDefs the <code>TaskDef</code>s for requested tasks
   *  @return an array of <code>Task</code>s
   *  @throws IllegalStateException if invoked after <code>done</code> has been invoked.
   */
  def tasks(taskDefs: Array[TaskDef]): Array[_root_.sbt.testing.Task] =
    taskDefs map (PartestTask(_, args): _root_.sbt.testing.Task)

  /** Indicates the client is done with this <code>Runner</code> instance.
   *
   *  @return a possibly multi-line summary string, or the empty string if no summary is provided -- TODO
   */
  def done(): String = ""
}

/** Run partest in this VM. Assumes we're running in a forked VM!
 *
 * TODO: make configurable
 */
case class PartestTask(taskDef: TaskDef, args: Array[String]) extends Task {

  // Get scala version through test name
  val scalaVersion = taskDef.fullyQualifiedName.stripPrefix("partest-")

  /** Executes this task, possibly returning to the client new tasks to execute. */
  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val forkedCp    = scala.util.Properties.javaClassPath
    val classLoader = new URLClassLoader(forkedCp.split(java.io.File.pathSeparator).map(new File(_).toURI.toURL))

    if (Runtime.getRuntime().maxMemory() / (1024*1024) < 800)
      loggers foreach (_.warn(s"""Low heap size detected (~ ${Runtime.getRuntime().maxMemory() / (1024*1024)}M). Please add the following to your build.sbt: javaOptions in Test += "-Xmx1G""""))

    val maybeOptions =
      ScalaJSPartestOptions(args, str => loggers.foreach(_.error(str)))

    maybeOptions foreach { options =>
      val runner = SBTRunner(
          Framework.fingerprint, eventHandler, loggers,
          new File(s"../partest/fetchedSources/${scalaVersion}"),
          classLoader, null, null, Array.empty[String], Array("run", "pos", "neg"), options, scalaVersion)

      try runner.run()
      catch {
        case ex: ClassNotFoundException =>
          loggers foreach { l => l.error("Please make sure partest is running in a forked VM by including the following line in build.sbt:\nfork in Test := true") }
          throw ex
      }
    }

    Array()
  }

  type SBTRunner = { def run(): Unit }

  // use reflection to instantiate scala.tools.partest.scalajs.ScalaJSSBTRunner,
  // casting to the structural type SBTRunner above so that method calls on the result will be invoked reflectively as well
  private def SBTRunner(partestFingerprint: Fingerprint, eventHandler: EventHandler, loggers: Array[Logger], testRoot: File, testClassLoader: URLClassLoader, javaCmd: File, javacCmd: File, scalacArgs: Array[String], args: Array[String], options: ScalaJSPartestOptions, scalaVersion: String): SBTRunner = {
    val runnerClass = Class.forName("scala.tools.partest.scalajs.ScalaJSSBTRunner")
    runnerClass.getConstructors()(0).newInstance(partestFingerprint, eventHandler, loggers, testRoot, testClassLoader, javaCmd, javacCmd, scalacArgs, args, options, scalaVersion).asInstanceOf[SBTRunner]
  }

  /** A possibly zero-length array of string tags associated with this task. */
  def tags: Array[String] = Array()
}
