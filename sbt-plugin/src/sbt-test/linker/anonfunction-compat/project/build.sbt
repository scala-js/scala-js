val scalaJSVersion = sys.props("plugin.version")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

/* Workaround: we use scalajs-linker_2.13 in sbt2 because linker is not yet
 * published for Scala 3.
 * We must exclude linker-interface, ir, and logging because:
 *
 * 1. In sbt 2.x, the sbt plugin is compiled with Scala 3, so it depends on
 *    linker-interface_3, ir_3, and logging_3.
 * 2. scalajs-linker_2.13 transitively depends on the _2.13 versions of these.
 * 3. Causes dependency conflicts (both _3 and _2.13 versions on classpath).
 *
 * By excluding those artifacts, FilteringClassLoader should delegate to
 * Scala3 compiled artifacts for those classes at runtime, which should
 * be binary compatible.
 *
 * It shouldn't be a problem for regular Scala.js project,
 * as long as they don't use linker directly.
 */
libraryDependencies += ("org.scala-js" %% "scalajs-linker" % scalaJSVersion)
  .cross(CrossVersion.for3Use2_13)
  .exclude("org.scala-js", "scalajs-linker-interface_2.13")
  .exclude("org.scala-js", "scalajs-ir_2.13")
  .exclude("org.scala-js", "scalajs-logging_2.13")

