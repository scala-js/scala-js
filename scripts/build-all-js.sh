#! /bin/sh

tasks="fastOptJS fullOptJS"
projects="helloworld/ reversi/ testingExample/test: testSuite/test:"

for v in 2.11.7 2.10.6 2.12.0-M2; do
    echo "++$v"
    echo "package"

    for p in $projects; do
        for t in $tasks; do
            echo "$p$t"
        done
    done
done | sbt
