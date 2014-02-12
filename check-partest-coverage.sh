#! /bin/sh

TMP_PREF=`basename $0`
TMP_HAVE_FILE=`mktemp /tmp/${TMP_PREF}_have_XXXXX` || exit 2
TMP_KNOW_FILE=`mktemp /tmp/${TMP_PREF}_know_XXXXX` || exit 2
BASEDIR="`dirname $0`"
TESTDIR="$BASEDIR/scalalib/source-2.11/test/files"
KNOWDIR="$BASEDIR/partest-suite/src/test/resources/scala/tools/partest/scalajs"

# Trap removal of tmp files on exit
trap "rm \"$TMP_HAVE_FILE\" \"$TMP_KNOW_FILE\"" EXIT

# Find all partests
( # Subshell to protect cwd
cd "$TESTDIR"
find "run" "neg" "pos" \
    -mindepth 1 -maxdepth 1 \( -type d -or -name '*.scala' \) \
  | sort >> $TMP_HAVE_FILE
)

# Find classified partests
( # Subshell to protect cwd
cd "$KNOWDIR"
cat BlacklistedTests.txt BuglistedTests.txt WhitelistedTests.txt \
  | grep -E -v '^#|^\s*$' | sort >> $TMP_KNOW_FILE
)

diff -U 0 --label 'Classified Tests' $TMP_KNOW_FILE --label 'Existing Tests' $TMP_HAVE_FILE
