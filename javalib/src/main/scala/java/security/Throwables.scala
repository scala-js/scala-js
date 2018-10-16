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

package java.security

import java.lang.SecurityException

class AccessControlException(s: String, p: Permission)
    extends SecurityException(s) {

  def this(s: String) = this(s, null)

  def getPermission(): Permission = p
}
