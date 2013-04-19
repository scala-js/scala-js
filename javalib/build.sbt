sourcesJS <<= (baseDirectory, baseDirectory in library) map {
  (base, libraryBase) =>
    ((base / "source" / "src") ** "*.scala").get ++
    ((libraryBase / "src" / "main" / "scala" / "scala" / "js") ** "*.scala").get
}

sources in Compile := Seq()
