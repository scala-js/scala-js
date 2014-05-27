@ECHO OFF
set SCALA_BIN_VER=@SCALA_BIN_VER@
set SCALAJS_VER=@SCALAJS_VER@

for /F "tokens=5" %%i in (' scala -version 2^>^&1 1^>nul ') do set SCALA_VER=%%i

if NOT "%SCALA_VER:~0,4%" == "%SCALA_BIN_VER%" (
  echo "This bundle of Scala.js CLI is for %SCALA_BIN_VER%. Your scala version is %SCALA_VER%!" 1>&2
) else (
  set PLUGIN=%~dp0\..\lib\scalajs-compiler_%SCALA_VER%-%SCALAJS_VER%.jar
  set JSLIB=%~dp0\..\lib\scalajs-library_%SCALA_BIN_VER%-%SCALAJS_VER%.jar

  scalac -classpath "%JSLIB%" "-Xplugin:%PLUGIN%" %*
)
