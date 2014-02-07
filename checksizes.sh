#!/bin/sh

REVERSI_EXTDEPS_SIZE=$(stat '-c%s' examples/reversi/target/scala-2.11.0-M7/reversi-extdeps.js)
REVERSI_SELF_SIZE=$(stat '-c%s' examples/reversi/target/scala-2.11.0-M7/reversi.js)
REVERSI_OPT_SIZE=$(stat '-c%s' examples/reversi/target/scala-2.11.0-M7/reversi-opt.js)
echo "Reversi extdeps size = $REVERSI_EXTDEPS_SIZE"
echo "Reversi self size = $REVERSI_SELF_SIZE"
echo "Reversi opt size = $REVERSI_OPT_SIZE"
[ "$REVERSI_EXTDEPS_SIZE" -le 20200000 ] && [ "$REVERSI_SELF_SIZE" -le 76000 ] && [ "$REVERSI_OPT_SIZE" -le 275000 ]
