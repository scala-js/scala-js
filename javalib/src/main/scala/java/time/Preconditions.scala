package java.time

private[time] object Preconditions {
  // Like scala.Predef.require, but throws a DateTimeException.
  def requireDateTime(requirement: Boolean, message: => Any): Unit = {
    if (!requirement)
      throw new DateTimeException(message.toString)
  }
}
