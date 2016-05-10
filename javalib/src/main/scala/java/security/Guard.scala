package java.security

trait Guard {
  def checkGuard(o: Any): Unit
}
