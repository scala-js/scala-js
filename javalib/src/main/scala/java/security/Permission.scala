package java.security

abstract class Permission(name: String) extends Guard with Serializable {
  //def checkGuard(a: Any): Unit
  def implies(p: Permission): Boolean
  def equals(obj: Any): Boolean
  def hashCode(): Int
  //final def getName(): String
  def getActions(): String
  //def newPermissionCollection(): PermissionCollection
  override def toString(): String =
    s"ClassName ${this.getClass.getName} ${getActions}"
}
