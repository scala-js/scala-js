sourcesJS <<= (baseDirectory, baseDirectory in library) map {
  (base, libraryBase) =>
    ((base / "source" / "src") ** "*.scala").get ++ Seq(
        libraryBase / "src" / "main" / "scala" / "scala" / "js" / "JSAny.scala"
    )
}

sources in Compile := Seq()
