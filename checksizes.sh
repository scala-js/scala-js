#!/bin/sh

REVERSI_EXTDEPS_SIZE=$(stat '-c%s' examples/reversi/target/scala-2.10/reversi-extdeps.js)
REVERSI_SELF_SIZE=$(stat '-c%s' examples/reversi/target/scala-2.10/reversi.js)
REVERSI_OPT_SIZE=$(stat '-c%s' examples/reversi/target/scala-2.10/reversi-opt.js)
echo "Reversi extdeps size = $REVERSI_EXTDEPS_SIZE"
echo "Reversi self size = $REVERSI_SELF_SIZE"
echo "Reversi opt size = $REVERSI_OPT_SIZE"
[ "$REVERSI_EXTDEPS_SIZE" -le 18000000 ] && [ "$REVERSI_SELF_SIZE" -le 85000 ] && [ "$REVERSI_OPT_SIZE" -le 285000 ]
