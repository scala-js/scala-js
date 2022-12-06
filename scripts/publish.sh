#! /bin/sh

if [ $# -eq 1 -a "$1" = "-x" ]; then
    CMD="sbt"
else
    echo "Showing commands that would be executed. Use -x to run."
    CMD="echo sbt"
fi

SUFFIXES="2_12 2_13"

JAVA_LIBS="javalibintf javalib"
COMPILER="compiler jUnitPlugin"
JS_LIBS="library irJS linkerInterfaceJS linkerJS testInterface testBridge jUnitRuntime"
JVM_LIBS="ir linkerInterface linker testAdapter"
SCALA_LIBS="$JS_LIBS $JVM_LIBS"

# Publish Java libraries
ARGS=""
for p in $JAVA_LIBS; do
    ARGS="$ARGS $p/publishSigned"
done
$CMD $ARGS

# Publish compiler
for s in $SUFFIXES; do
    ARGS=""
    for p in $COMPILER; do
        ARGS="$ARGS +$p$s/publishSigned"
    done
    $CMD $ARGS
done

# Publish Scala libraries
for s in $SUFFIXES; do
    ARGS=""
    for p in $SCALA_LIBS; do
        ARGS="$ARGS $p$s/publishSigned"
    done
    $CMD $ARGS
done

# Publish sbt-plugin
$CMD sbtPlugin/publishSigned
