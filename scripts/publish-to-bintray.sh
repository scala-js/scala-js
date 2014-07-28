#! /bin/sh

if [ $# -eq 1 -a "$1" = "-x" ]; then
    CMD=sbt
else
    echo "Showing commands that would be issued to SBT. Use -x to run"
    CMD=cat
fi

# Subshell to generate SBT commands
(
    SET_BINTRAY="set PublishToBintray.publishToBintraySettings ++ Seq(publishMavenStyle := false)"
    FULL_VERSIONS="2.10.2 2.10.3 2.10.4 2.11.0 2.11.1 2.11.2"
    BIN_VERSIONS="2.10.4 2.11.2"
    SBT_VERSION="2.10.4"

    LIBS="library javalibEx jasmineTestFramework tools testBridge"

    # Publish compiler
    echo "project compiler"
    echo $SET_BINTRAY
    for v in $FULL_VERSIONS; do
        echo "++$v"
        echo "publish"
    done

    # Package libraries
    for p in $LIBS; do
        echo "project $p"
        echo $SET_BINTRAY
        for v in $BIN_VERSIONS; do
            echo "++$v"
            echo "publish"
        done
    done

    # Publish sbt-plugin
    echo "project sbtPlugin"
    echo $SET_BINTRAY
    echo "++$SBT_VERSION"
    echo "publish"

) | $CMD
