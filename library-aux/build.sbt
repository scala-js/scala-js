sourcesJS <<= baseDirectory map (base => ((base / "src" / "main" / "scala") ** "*.scala").get)

sources in Compile := Seq()
