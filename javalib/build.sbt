sourcesJS <<= baseDirectory map (base => ((base / "source" / "src") ** "*.scala").get)

sources in Compile := Seq()
