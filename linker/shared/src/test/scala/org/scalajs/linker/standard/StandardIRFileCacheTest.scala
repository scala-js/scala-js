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

package org.scalajs.linker.standard

import scala.concurrent._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.junit.async._

import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.Trees.ClassDef
import org.scalajs.ir.Names.{ClassName, ObjectClass}
import org.scalajs.ir.Version.Unversioned

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class StandardIRFileCacheTest {
  import scala.concurrent.ExecutionContext.{global => globalEc}
  import StandardIRFileCacheTest._

  val maxConcurrentReads = 5

  /** simulates a cache call and aggressive read of all files. */
  private def readAll(containers: List[IRContainer])(
      implicit ec: ExecutionContext): Future[_] = {
    val config = IRFileCacheConfig()
      .withMaxConcurrentReads(maxConcurrentReads)

    val irCache = (new StandardIRFileCache(config)).newCache

    irCache.cached(containers).flatMap { files =>
      Future.traverse(files)(file => IRFileImpl.fromIRFile(file).tree)
    }
  }

  @Test
  def testThrottlesConcurrentReads(): AsyncResult = await {
    val testEc = new TestExecutionContext(globalEc)

    val containers = List.tabulate(20)(i => new MockIRContainer(path = f"C$i"))

    val result = readAll(containers)(testEc)

    val ops = containers.flatMap(_.ops)

    def loop(): Future[Unit] = {
      testEc.runAll().flatMap { _ =>
        if (result.isCompleted) {
          Future.successful(())
        } else {
          val reading = ops.filter(_.running)
          assert(reading.nonEmpty, "caching is not completed but nothing is reading")
          assert(reading.size <= maxConcurrentReads, "reading too many files at the same time")

          reading.head.complete()

          loop()
        }
      } (globalEc)
    }

    loop()
  }
}

object StandardIRFileCacheTest {
  final class MockIRContainer(path: String)
      extends IRContainerImpl(path, Unversioned) {
    private val files = List.tabulate(10)(i => new MockIRFile(f"$path.F$i"))

    private val _sjsirFiles = new MockOperation(files)

    def ops: List[MockOperation[_]] = _sjsirFiles :: files.flatMap(_.ops)

    def sjsirFiles(implicit ec: ExecutionContext): Future[List[IRFile]] =
      _sjsirFiles.run()
  }

  final class MockIRFile(path: String) extends IRFileImpl(path, Unversioned) {
    private val className: ClassName = path

    private val _entryPointsInfo =
      new MockOperation(new EntryPointsInfo(className, false))

    private val _tree =
      new MockOperation(classDef(className, superClass = Some(ObjectClass)))

    def ops: List[MockOperation[_]] = List(_entryPointsInfo, _tree)

    def entryPointsInfo(implicit ec: ExecutionContext): Future[EntryPointsInfo] =
      _entryPointsInfo.run()

    def tree(implicit ec: ExecutionContext): Future[ClassDef] =
      _tree.run()
  }

  final class MockOperation[T](v: T) {
    private[this] var _running = false
    private[this] val _promise = Promise[T]()

    def running: Boolean = synchronized { _running }
    def completed: Boolean = synchronized { _promise.isCompleted }

    def complete(): Unit = synchronized {
      assert(running, "trying to complete an operation that isn't running")
      _running = false
      _promise.success(v)
    }

    def run(): Future[T] = synchronized {
      assert(!running, "operation started twice concurrently")
      assert(!completed, "operation started twice consecutively")
      _running = true
      _promise.future
    }
  }

  /** An ExecutionContext that only executes tasks when [[runAll]] is called. */
  final class TestExecutionContext(underlying: ExecutionContext)
      extends ExecutionContext {
    private var tasks: List[Runnable] = Nil
    private var failureCause: Throwable = _

    override def execute(run: Runnable): Unit = synchronized {
      tasks ::= run
    }

    override def reportFailure(cause: Throwable): Unit = synchronized {
      if (failureCause != null)
        failureCause = cause
    }

    def runAll(): Future[Unit] = synchronized {
      if (failureCause != null) {
        Future.failed(failureCause)
      } else {
        implicit val ec = underlying

        val taskSnapshot = tasks
        tasks = Nil

        if (taskSnapshot.isEmpty) {
          Future.successful(())
        } else {
          Future.traverse(taskSnapshot)(task => Future(task.run()))
            .flatMap(_ => runAll())
        }
      }
    }
  }
}
