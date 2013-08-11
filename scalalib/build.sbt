unmanagedSourceDirectories in Compile <<= baseDirectory(base =>
    Seq(base / "source" / "src" / "library",
        base / "source" / "src" / "continuations" / "library")
)
