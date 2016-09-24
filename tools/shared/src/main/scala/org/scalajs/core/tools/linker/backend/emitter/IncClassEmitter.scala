/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2016, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.tools.linker.backend.emitter

import scala.collection.mutable

import org.scalajs.core.tools.sem._

import org.scalajs.core.tools.linker.backend.OutputMode
import org.scalajs.core.tools.linker.LinkingUnit

/** Tracks non-local dependencies that need to be invalidated in incremental re-runs.
 *  For example, whether a class inlines its `init` method in its JS constructor.
 *
 *  The only reason this is not `private[emitter]` is because `RhinoJSEnv`
 *  needs it.
 */
private[scalajs] final class IncClassEmitter(semantics: Semantics,
    outputMode: OutputMode, internalOptions: InternalOptions) {
  import IncClassEmitter._

  def this(semantics: Semantics, outputMode: OutputMode) =
    this(semantics, outputMode, InternalOptions())

  private var currentClassesCtorOpt: Set[String] = Set.empty
  private[this] val askedForCtorOpt =
    mutable.Map.empty[String, mutable.Set[InvalidatableCache]]

  def beginRun(unit: LinkingUnit): ScalaJSClassEmitter = {
    val classEmitter = new ScalaJSClassEmitter(outputMode,
        internalOptions, unit, this)

    val newClassesCtorOpt = classEmitter.candidateForJSConstructorOpt

    val changedClasses = {
      (currentClassesCtorOpt -- newClassesCtorOpt) ++
      (newClassesCtorOpt -- currentClassesCtorOpt)
    }
    currentClassesCtorOpt = newClassesCtorOpt

    askedForCtorOpt.retain { (className, callers) =>
      if (changedClasses(className)) {
        // invalidate
        callers.foreach(_.invalidate())
        false
      } else {
        true
      }
    }

    classEmitter
  }

  def usesJSConstructorOpt(className: String): Boolean = {
    currentClassesCtorOpt(className)
  }

  def usesJSConstructorOpt(className: String,
      caller: InvalidatableCache): Boolean = {
    val currentEntry = askedForCtorOpt.getOrElseUpdate(className,
        mutable.Set.empty)
    currentEntry += caller

    usesJSConstructorOpt(className)
  }
}

private[scalajs] object IncClassEmitter {
  trait InvalidatableCache {
    def invalidate(): Unit
  }
}
