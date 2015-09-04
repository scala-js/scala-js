# Launcher script for task worker (dispatched by matrix-build)

# Setup sbt home dirs

LOC_SBT_BASE=/localhome/jenkins/scala-js-sbt-homes
LOC_SBT_BOOT=$LOC_SBT_BASE/sbt-boot
LOC_SBT_HOME=$LOC_SBT_BASE/sbt-home

mkdir -p $LOC_SBT_BOOT
mkdir -p $LOC_SBT_HOME

### Create tmp.sh ###

# Node module path

echo 'export NODE_PATH="$HOME/node_modules/"' > tmp.sh

# sbt options

echo 'SBT_OPTS="' \
    "-J-Xmx3G -J-XX:MaxPermSize=512M" \
    "-Djline.terminal=jline.UnsupportedTerminal" \
    "-Dsbt.boot.directory=$LOC_SBT_BOOT" \
    "-Dsbt.ivy.home=$LOC_SBT_HOME" \
    "-Divy.home=$LOC_SBT_HOME" \
    "-Dsbt.global.base=$LOC_SBT_BASE" \
    '"' >> tmp.sh

# Define setJavaVersion

echo 'setJavaVersion() {' >> tmp.sh
echo '  export JAVA_HOME=$HOME/apps/java-$1' >> tmp.sh
echo '}' >> tmp.sh

# Define sbt and sbtretry

echo 'sbtretry() {' >> tmp.sh
echo '  local TIMEOUT=25m' >> tmp.sh
echo '  echo "RUNNING timeout -k 5 $TIMEOUT sbt" "$@"' >> tmp.sh
echo '  timeout -k 5 $TIMEOUT sbt $SBT_OPTS "$@"' >> tmp.sh
echo '  local CODE=$?' >> tmp.sh
echo '  if [ "$CODE" -eq 124 ]; then' >> tmp.sh
echo '    echo "TIMEOUT after" $TIMEOUT' >> tmp.sh
echo '  fi' >> tmp.sh
echo '  if [ "$CODE" -ne 0 ]; then' >> tmp.sh
echo '    echo "RETRYING timeout -k 5 $TIMEOUT sbt" "$@"' >> tmp.sh
echo '    timeout -k 5 $TIMEOUT sbt $SBT_OPTS "$@"' >> tmp.sh
echo '    CODE=$?' >> tmp.sh
echo '    if [ "$CODE" -eq 124 ]; then' >> tmp.sh
echo '      echo "TIMEOUT after" $TIMEOUT' >> tmp.sh
echo '    fi' >> tmp.sh
echo '    if [ "$CODE" -ne 0 ]; then' >> tmp.sh
echo '      echo "FAILED TWICE"' >> tmp.sh
echo '      return $CODE' >> tmp.sh
echo '    fi' >> tmp.sh
echo '  fi' >> tmp.sh
echo '}' >> tmp.sh

echo 'alias sbt="sbt $SBT_OPTS"' >> tmp.sh

# Add the actual command

echo "$taskCommand" >> tmp.sh

# Run

sh tmp.sh
