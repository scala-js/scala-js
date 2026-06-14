#! /bin/sh

if [ $# -eq 1 -a "$1" = "-x" ]; then
    CMD="sbt"
    EXECUTING='1'
else
    echo "Showing commands that would be executed. Use -x to run."
    CMD="echo sbt"
    EXECUTING=''
fi

if [ $EXECUTING ]; then
    if [ -z "$SONATYPE_USERNAME$SONATYPE_PASSWORD" ]; then
        echo "Please set the SONATYPE_USERNAME and SONATYPE_PASSWORD variables."
        exit 1
    fi
fi

SUFFIXES="2_12 2_13"
SUFFIXES_WITH_3="2_12 2_13 3"

JAVA_LIBS="javalibintf javalib"
FULL_SCALA_LIBS="compiler jUnitPlugin scalalib"
SCALA_2_LIBS="library testInterface testBridge jUnitRuntime irJS linkerInterfaceJS linkerJS"
SCALA_2_3_LIBS="ir linkerInterface linker testAdapter"

# Publish Java libraries
ARGS=""
for p in $JAVA_LIBS; do
    ARGS="$ARGS $p/publishSigned"
done
$CMD $ARGS

# Publish artifacts built with the full Scala version
for s in $SUFFIXES; do
    ARGS=""
    for p in $FULL_SCALA_LIBS; do
        ARGS="$ARGS +$p$s/publishSigned"
    done
    $CMD $ARGS
done

# Publish Scala 2 libraries
for s in $SUFFIXES; do
    ARGS=""
    for p in $SCALA_2_LIBS; do
        ARGS="$ARGS $p$s/publishSigned"
    done
    $CMD $ARGS
done

# Publish Scala 2 and 3 libraries
for s in $SUFFIXES_WITH_3; do
    ARGS=""
    for p in $SCALA_2_3_LIBS; do
        ARGS="$ARGS $p$s/publishSigned"
    done
    $CMD $ARGS
done

# Publish sbt-plugin
$CMD sbtPlugin2_12/publishSigned sbtPlugin3/publishSigned

if [ $EXECUTING ]; then
    echo "All done."
    echo "If you're publishing a non-snapshot release, now you need to execute:"
    echo "  sbt sonaUpload"
    echo "then go to https://central.sonatype.com/publishing,"
    echo "double-check the contents, and click 'Publish'."
fi
