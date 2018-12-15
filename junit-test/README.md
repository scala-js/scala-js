JUnit tests compare the output of a JUnit test to a recording.

The recordings lie in the `outputs` directory. To re-record all tests, set the
`org.scalajs.junit.utils.record` system property and run the JVM tests:

```
sbt -Dorg.scalajs.junit.utils.record jUnitTestOutputsJVM/test
```
