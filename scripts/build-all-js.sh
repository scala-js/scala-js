#! /bin/sh

tasks="packageJS fastOptJS fullOptJS"
projects="helloworld/ reversi/ testingExample/test: testSuite/test:"

for v in 2.11.1 2.10.4; do
    echo "++$v"
    echo "package"

    for p in $projects; do
        for t in $tasks; do
            echo "$p$t"
        done
    done
done | sbt
