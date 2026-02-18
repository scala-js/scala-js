name := "change-config-and-source"

scalaVersion := "2.12.21"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

val copyFastLinkOutput = inputKey[Unit]("Copy fastLinkJS main.js to destination")
val checkFastLinkNewer = inputKey[Unit]("Check if fastLinkJS main.js is newer than file")
val checkFastLinkMirror = inputKey[Unit]("Check if fastLinkJS main.js mirrors file")

copyFastLinkOutput := {
  import sbt.complete.DefaultParsers._
  val dest = spaceDelimited("<dest>").parsed.head
  val outputDir = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
  val source = outputDir / "main.js"
  val target = (Compile / fastLinkJS / crossTarget).value / dest
  IO.copyFile(source, target)
}

checkFastLinkNewer := {
  import sbt.complete.DefaultParsers._
  val other = spaceDelimited("<other>").parsed.head
  val outputDir = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
  val source = outputDir / "main.js"
  val target = (Compile / fastLinkJS / crossTarget).value / other
  assert(source.lastModified > target.lastModified,
    s"$source is not newer than $target")
}

checkFastLinkMirror := {
  import sbt.complete.DefaultParsers._
  val other = spaceDelimited("<other>").parsed.head
  val outputDir = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
  val source = outputDir / "main.js"
  val target = (Compile / fastLinkJS / crossTarget).value / other
  assert(IO.readBytes(source).sameElements(IO.readBytes(target)),
    s"$source does not mirror $target")
}
