#! /bin/sh

if [ $# -eq 1 -a "$1" = "-x" ]; then
    CMD="sbt"
else
    echo "Showing commands that would be executed. Use -x to run."
    CMD="echo sbt"
fi

COMPILER_VERSIONS="2.11.0 2.11.1 2.11.2 2.11.4 2.11.5 2.11.6 2.11.7 2.11.8 2.11.11 2.11.12 2.12.1 2.12.2 2.12.3 2.12.4 2.12.5 2.12.6 2.13.0-M5"
BIN_VERSIONS="2.11.12 2.12.6"
NO_TOOLS_BIN_VERSIONS="2.13.0-M5"
JVM_BIN_VERSIONS="2.10.7 2.11.12 2.12.6 2.13.0-M5"
SBT013_VERSION="2.10.7"
SBT013_SBTVERSION="0.13.17"
SBT1_VERSION="2.12.6"
SBT1_SBTVERSION="1.0.0"

COMPILER="compiler jUnitPlugin"
LIBS="library irJS ioJS loggingJS linkerJS testInterface jUnitRuntime"
NO_TOOLS_LIBS="library testInterface jUnitRuntime"
JVM_LIBS="ir io logging linker jsEnvs jsEnvsTestKit nodeJSEnv testAdapter"

# Publish compiler
for v in $COMPILER_VERSIONS; do
    ARGS="++$v"
    for p in $COMPILER; do
        ARGS="$ARGS $p/publishSigned"
    done
    $CMD $ARGS
done

# Publish libraries
for v in $BIN_VERSIONS; do
    ARGS="++$v"
    for p in $LIBS; do
        ARGS="$ARGS $p/publishSigned"
    done
    $CMD $ARGS
done

# Publish limited versions
for v in $NO_TOOLS_BIN_VERSIONS; do
    ARGS="++$v"
    for p in $NO_TOOLS_LIBS; do
        ARGS="$ARGS $p/publishSigned"
    done
    $CMD $ARGS
done

# Publish JVM libraries
for v in $JVM_BIN_VERSIONS; do
    ARGS="++$v"
    for p in $JVM_LIBS; do
        ARGS="$ARGS $p/publishSigned"
    done
    $CMD $ARGS
done

# Publish sbt-plugin
$CMD "++$SBT013_VERSION" "^^$SBT013_SBTVERSION" "sbtPlugin/publishSigned"
$CMD "++$SBT1_VERSION" "^^$SBT1_SBTVERSION" "sbtPlugin/publishSigned"
