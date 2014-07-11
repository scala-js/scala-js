package scala.scalajs.tools.io

object CacheUtils {

  def joinVersions(vs: Option[String]*): Option[String] = {
    val bld = new StringBuilder

    @scala.annotation.tailrec
    def loop(vs: Seq[Option[String]]): Option[String] = {
      vs match {
        case Some(v) :: vss =>
          bld.append(mangleVersionString(v))
          loop(vss)
        case None :: _ =>
          None
        case Nil =>
          Some(bld.toString)
      }
    }

    loop(vs.toList)
  }

  def joinVersions(vs: String*): String =
    vs.map(mangleVersionString _).mkString

  private def mangleVersionString(str: String) = s"${str.length}:$str"

  def cached(version: Option[String],
      cache: Option[WritableVirtualTextFile])(action: => Unit): Unit = {

    val upToDate = for {
      v <- version
      c <- cache if c.exists
    } yield c.content == v

    // Are we outdated?
    if (!upToDate.getOrElse(false)) {
      action

      // Write cache
      for (c <- cache; v <- version) {
        val w = c.contentWriter
        try w.write(v)
        finally w.close()
      }
    }
  }

}
