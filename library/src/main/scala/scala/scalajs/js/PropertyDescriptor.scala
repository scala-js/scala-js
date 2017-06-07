/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */



package scala.scalajs.js

import scala.scalajs.js

trait PropertyDescriptor extends js.Object {
  // All kinds of property descriptors

  var configurable: js.UndefOr[Boolean] = js.undefined
  var enumerable: js.UndefOr[Boolean] = js.undefined

  // Data descriptors

  var value: js.UndefOr[scala.Any] = js.undefined
  var writable: js.UndefOr[Boolean] = js.undefined

  // Accessor descriptors

  var get: js.UndefOr[js.Function0[scala.Any]] = js.undefined
  var set: js.UndefOr[js.Function1[scala.Any, scala.Any]] = js.undefined
}
