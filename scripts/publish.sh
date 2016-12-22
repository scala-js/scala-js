#! /bin/sh

if [ $# -eq 1 -a "$1" = "-x" ]; then
    export PUBLISH_TO_BINTRAY=true
    CMD="sbt"
else
    echo "Showing commands that would be executed. Use -x to run."
    CMD="echo sbt"
fi

FULL_VERSIONS="2.10.2 2.10.3 2.10.4 2.10.5 2.10.6 2.11.0 2.11.1 2.11.2 2.11.4 2.11.5 2.11.6 2.11.7 2.11.8 2.12.0 2.12.1"
BIN_VERSIONS="2.10.6 2.11.8 2.12.1"
CLI_VERSIONS="2.10.6 2.11.8 2.12.1"
SBT_VERSION="2.10.6"

COMPILER="compiler jUnitPlugin"
LIBS="library javalibEx ir irJS tools toolsJS jsEnvs jsEnvsTestKit testAdapter stubs testInterface jUnitRuntime"

# Publish compiler
for v in $FULL_VERSIONS; do
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

# Publish the CLI
for v in $CLI_VERSIONS; do
    $CMD "++$v" "cli/publishSigned"
done

# Publish sbt-plugin
$CMD "++$SBT_VERSION" "sbtPlugin/publishSigned"
