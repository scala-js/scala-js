/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.emitter

import org.scalajs.core.ir.Trees.{FieldDef, JSNativeLoadSpec}

private[emitter] trait GlobalKnowledge {
  /** Tests whether the parent class data is accessed in the linking unit. */
  def isParentDataAccessed: Boolean

  // TODO Get rid of this when we break backward binary compatibility
  /** Whether the standard library we're using has the new `RuntimeLong`
   *  implementation, with `lo` and `hi`.
   */
  def hasNewRuntimeLong: Boolean

  /** Tests whether the specified class name refers to an `Interface`. */
  def isInterface(className: String): Boolean

  /** `None` for non-native JS classes/objects; `Some(spec)` for native JS
   *  classes/objects.
   *
   *  It is invalid to call this method with a class that is not a JS class
   *  or object (native or not).
   */
  def getJSNativeLoadSpec(className: String): Option[JSNativeLoadSpec]

  /** The `encodedName` of the superclass of a (non-native) JS class.
   *
   *  It is invalid to call this method with a class that is not a non-native
   *  JS class.
   */
  def getSuperClassOfJSClass(className: String): String

  /** The `FieldDef`s of a non-native JS class.
   *
   *  It is invalid to call this method with a class that is not a non-native
   *  JS class.
   */
  def getJSClassFieldDefs(className: String): List[FieldDef]
}
