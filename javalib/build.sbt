unmanagedSourceDirectories in Compile <<= (
    baseDirectory, baseDirectory in library
) apply {
  (base, libraryBase) => Seq(
      base / "source" / "src",
      libraryBase / "src" / "main" / "scala" / "scala" / "js"
  )
}
