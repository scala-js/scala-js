#! /bin/sh

if [ $# -eq 1 -a "$1" = "-x" ]; then
    export PUBLISH_TO_BINTRAY=true
    CMD="sbt"
else
    echo "Showing commands that would be executed. Use -x to run."
    CMD="echo sbt"
fi

FULL_VERSIONS="2.10.2 2.10.3 2.10.4 2.10.5 2.11.0 2.11.1 2.11.2 2.11.4 2.11.5 2.11.6"
BIN_VERSIONS="2.10.5 2.11.6"
SBT_VERSION="2.10.5"

LIBS="library javalibEx ir irJS tools toolsJS jsEnvs testAdapter stubs testInterface"

# Publish compiler
ARGS=""
for v in $FULL_VERSIONS; do
    ARGS="$ARGS ++$v compiler/publishSigned"
done
$CMD $ARGS

# Package libraries
for v in $BIN_VERSIONS; do
    ARGS="++$v"
    for p in $LIBS; do
        ARGS="$ARGS $p/publishSigned"
    done
    $CMD $ARGS
done

# Publish sbt-plugin
$CMD "++$SBT_VERSION" "sbtPlugin/publishSigned"
