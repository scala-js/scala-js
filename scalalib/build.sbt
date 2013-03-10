sourcesJS <<= baseDirectory map (base => ((base / "source" / "src" / "library") ** "*.scala").get)

sources in Compile := Seq()
