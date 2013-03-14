sourcesJS <<= baseDirectory map (base => (base * "*.scala").get)

sources in Compile := Seq()
