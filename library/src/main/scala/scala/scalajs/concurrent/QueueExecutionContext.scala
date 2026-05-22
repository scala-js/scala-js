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

package scala.scalajs.concurrent

import scala.collection.mutable
import scala.concurrent.ExecutionContextExecutor

import scala.scalajs.js
import scala.scalajs.js.|
import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind}
import scala.scalajs.LinkingInfo.ModuleKind.MinimalWasmModule

object QueueExecutionContext {
  def timeouts(): ExecutionContextExecutor =
    new TimeoutsExecutionContext

  def promises(): ExecutionContextExecutor =
    new PromisesExecutionContext

  def single(): ExecutionContextExecutor =
    new SingleThreadedExecutionContext

  def apply(): ExecutionContextExecutor = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      single()
    } {
      if (js.typeOf(js.Dynamic.global.Promise) == "undefined") timeouts()
      else promises()
    }
  }

  private final class SingleThreadedExecutionContext extends ExecutionContextExecutor {
    private val tasks = mutable.ListBuffer.empty[Runnable]

    private var running: Boolean = false

    def execute(runnable: Runnable): Unit = {
      tasks += runnable

      if (!running) {
        running = true
        try {
          while (tasks.nonEmpty) {
            val task = tasks.remove(0)
            try {
              task.run()
            } catch {
              case t: Throwable => reportFailure(t)
            }
          }
        } finally {
          running = false
        }
      }
    }

    def reportFailure(t: Throwable): Unit =
      () // t.printStackTrace() // We don't even have a standard output in MinimalWasm
  }

  private final class TimeoutsExecutionContext extends ExecutionContextExecutor {
    def execute(runnable: Runnable): Unit = {
      js.Dynamic.global.setTimeout({ () =>
        try {
          runnable.run()
        } catch {
          case t: Throwable => reportFailure(t)
        }
      }, 0)
    }

    def reportFailure(t: Throwable): Unit =
      t.printStackTrace()
  }

  private final class PromisesExecutionContext extends ExecutionContextExecutor {
    private val resolvedUnitPromise = js.Promise.resolve[Unit](())

    def execute(runnable: Runnable): Unit = {
      resolvedUnitPromise.`then` { (_: Unit) =>
        try {
          runnable.run()
        } catch {
          case t: Throwable => reportFailure(t)
        }
        (): Unit | js.Thenable[Unit]
      }
    }

    def reportFailure(t: Throwable): Unit =
      t.printStackTrace()
  }
}
