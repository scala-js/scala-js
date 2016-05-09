package java.security

import java.lang.SecurityException

class AccessControlException(s: String, p: Permission)
    extends SecurityException(s) {

  def this(s: String) = this(s, null)

  def getPermission(): Permission = p
}
