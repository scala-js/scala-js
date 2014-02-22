#!/bin/sh

FULLVER="$1"
case $FULLVER in
  2.10.3)
    VER=2.10
    ;;
  2.11.0-M8)
    VER=2.11.0-M8
    ;;
esac

REVERSI_EXTDEPS_SIZE=$(stat '-c%s' examples/reversi/target/scala-$VER/reversi-extdeps.js)
REVERSI_SELF_SIZE=$(stat '-c%s' examples/reversi/target/scala-$VER/reversi.js)
REVERSI_OPT_SIZE=$(stat '-c%s' examples/reversi/target/scala-$VER/reversi-opt.js)
echo "Reversi extdeps size = $REVERSI_EXTDEPS_SIZE"
echo "Reversi self size = $REVERSI_SELF_SIZE"
echo "Reversi opt size = $REVERSI_OPT_SIZE"
case $VER in
  2.10)
    REVERSI_EXTDEPS_EXPECTEDSIZE=23500000
    REVERSI_SELF_EXPECTEDSIZE=95000
    REVERSI_OPT_EXPECTEDSIZE=285000
    ;;
  2.11.0-M8)
    REVERSI_EXTDEPS_EXPECTEDSIZE=20500000
    REVERSI_SELF_EXPECTEDSIZE=76000
    REVERSI_OPT_EXPECTEDSIZE=275000
    ;;
esac
[ "$REVERSI_EXTDEPS_SIZE" -le "$REVERSI_EXTDEPS_EXPECTEDSIZE" ] && \
  [ "$REVERSI_SELF_SIZE" -le "$REVERSI_SELF_EXPECTEDSIZE" ] && \
  [ "$REVERSI_OPT_SIZE" -le "$REVERSI_OPT_EXPECTEDSIZE" ]
