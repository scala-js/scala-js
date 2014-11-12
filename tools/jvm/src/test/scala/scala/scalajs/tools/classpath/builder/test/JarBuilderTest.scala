package scala.scalajs.tools.classpath.builder.test

import scala.scalajs.tools.classpath.builder._

import org.junit.Test
import org.junit.Assert._

class JarBuilderTest {

  val jsFileContent = "window.alert('Hello World');"
  val jsFileName = "lib.js"

  private def getJARInputStream = {
    // Keep imports local so we don't accidentally use something we shouldn't
    import java.util.zip._
    import java.io._

    // Write a ZIP to a byte array
    val outBuf = new ByteArrayOutputStream
    val out = new ZipOutputStream(outBuf)

    out.putNextEntry(new ZipEntry(jsFileName))
    val w = new OutputStreamWriter(out, "UTF-8")
    w.write(jsFileContent)
    w.flush()

    out.close()

    new ByteArrayInputStream(outBuf.toByteArray)
  }

  /** Trivial FS that has only one jar */
  trait TrivialFS extends FileSystem {
    // Keep imports local so we don't accidentally use something we shouldn't
    import java.io.{InputStream, Reader}
    import scala.scalajs.tools.io._
    import scala.collection.immutable.Traversable

    type File = Unit

    val DummyVersion: String = "DUMMY"

    def isDirectory(f: File): Boolean = false
    def isFile(f: File): Boolean = true
    def isJSFile(f: File): Boolean = false
    def isIRFile(f: File): Boolean = false
    def isJARFile(f: File): Boolean = true
    def exists(f: File): Boolean = true

    def getName(f: File): String = "jar"
    def getAbsolutePath(f: File): String = "jar"
    def getVersion(f: File): String = ""

    def listFiles(d: File): Traversable[File] = ???

    def toJSFile(f: File): VirtualJSFile = ???
    def toIRFile(f: File): VirtualScalaJSIRFile = ???
    def toReader(f: File): Reader = ???
    def toInputStream(f: File): InputStream = getJARInputStream
  }

  class TestJARBuilder extends AbstractJarLibClasspathBuilder with TrivialFS

  @Test
  def readInMemoryJarClasspath {
    val builder = new TestJARBuilder
    val cp = builder.build(Seq(()))

    assertArrayEquals(
        Array[Object](jsFileName -> jsFileContent),
        cp.availableLibs.mapValues(_.content).toArray[Object])
  }

}
