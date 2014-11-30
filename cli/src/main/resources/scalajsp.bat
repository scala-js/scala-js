@ECHO OFF
set SCALA_BIN_VER=@SCALA_BIN_VER@
set SCALAJS_VER=@SCALAJS_VER@

set CLILIB="%~dp0\..\lib\scalajs-cli-assembly_%SCALA_BIN_VER%-%SCALAJS_VER%.jar"

scala -classpath %CLILIB% org.scalajs.cli.Scalajsp %*
