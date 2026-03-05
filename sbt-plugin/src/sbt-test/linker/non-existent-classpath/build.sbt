version := scalaJSVersion
scalaVersion := "2.12.21"

enablePlugins(ScalaJSPlugin)

/* It seems we can't test this in sbt2 because
 * in sbt2, fullClasspath is typed Seq[Attributed[HashedVirtualFileRef]]
 * However, HashedVirtualFileRef type contains
 * hash and size of the file content, if we add a non-existent file to fullClasspath,
 * it can't compute the hash, and serialization fails.
 * sjsonnew.SerializationException: error while writing LList (data, ${BASE}/non-existent-directory-please-dont-ever-create-this) :*: (metadata, Map()) :*: LNil
 */

// Test that non-existent classpath entries are allowed - #2198
Compile / fullClasspath += baseDirectory.value /
  "non-existent-directory-please-dont-ever-create-this"

scalaJSUseMainModuleInitializer := true
