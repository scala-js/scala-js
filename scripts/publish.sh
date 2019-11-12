#! /bin/sh

if [ $# -eq 1 -a "$1" = "-x" ]; then
    CMD="sbt"
else
    echo "Showing commands that would be executed. Use -x to run."
    CMD="echo sbt"
fi

SUFFIXES="2_11 2_12 2_13"

COMPILER="compiler jUnitPlugin"
LIBS="library irJS loggingJS linkerJS testInterface testBridge jUnitRuntime"
JVM_LIBS="ir logging linker jsEnvs jsEnvsTestKit nodeJSEnv testAdapter"

# Publish compiler
for s in $SUFFIXES; do
    ARGS=""
    for p in $COMPILER; do
        ARGS="$ARGS +$p$s/publishSigned"
    done
    $CMD $ARGS
done

# Publish libraries
for s in $SUFFIXES; do
    ARGS=""
    for p in $LIBS; do
        ARGS="$ARGS $p$s/publishSigned"
    done
    $CMD $ARGS
done

# Publish JVM libraries
for v in $SUFFIXES; do
    ARGS=""
    for p in $JVM_LIBS; do
        ARGS="$ARGS $p$s/publishSigned"
    done
    $CMD $ARGS
done

# Publish sbt-plugin
$CMD sbtPlugin/publishSigned
