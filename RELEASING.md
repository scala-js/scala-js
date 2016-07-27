# Scala.js Release Process

1. Clean up bugs:
    - Make sure all bugs assigned to the milestone are closed.
    - Make sure all fixed bugs are assigned to the milestone ([this query][1]
      should not return anything).
1. Create a "Version x.y.z." commit ([example][2]) and push it to a branch on
   your fork.
    1. Ping people on the commit for review.
    1. Once you have LGTM, push to master (do *not* create a merge commit).
1. Testing (post results as comments to commit):
    - Nightly
    - Weekly
    - [Manual testing][3]
1. If all tests pass, tag the commit with the release version.
1. Perform [manual testing][3] that needs the tagging (source maps).
1. Publish:
    - Sonatype, bintray (`./script/publish.sh`)
    - CLI to website (simple `scp`)
    - Docs to website: Use
      `~/fetchapis.sh <full sjs version> <binary sjs version>` on the webserver
      once artifacts are on maven central.
1. Once artifacts are on maven central, create a "Towards x.y.z." commit
   ([example][4]).
    1. Create a "DO NOT MERGE" PR for CI and review.
    1. Once you have LGTM, push the commit (do *not* click the merge button)
    1. Remove the "DO NOT MERGE" from the PR title (since it will now show as
       merged).
1. Prepare release announcement, taking the last one as model ([example][5]).
1. When merging the release announcement PR (after proper review):
    - Update the latest/ URLs (use `~/setlatestapi.sh <full sjs version>` on
      webserver)
    - Announce on Twitter using the @scala_js account
    - Announce on [Gitter](https://gitter.im/scala-js/scala-js)
    - Announce on the mailing list (scala-js@googlegroups.com)
    - Cross-post to scala-announce (scala-announce@googlegroups.com)

[1]: https://github.com/scala-js/scala-js/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20no%3Amilestone%20-label%3Ainvalid%20-label%3Aduplicate%20-label%3Aas-designed%20-label%3Aquestion%20-label%3Awontfix%20-label%3A%22can%27t%20reproduce%22%20-label%3A%22separate%20repo%22
[2]: https://github.com/scala-js/scala-js/commit/a09e8cdd92b962e90c83ec124b9764970a4889ff
[3]: https://github.com/scala-js/scala-js/blob/master/TESTING
[4]: https://github.com/scala-js/scala-js/commit/c51f8b65d3eca45de84397f7167058c91d6b6aa1
[5]: https://github.com/scala-js/scala-js-website/commit/8dc9e9d3ee63ec47e6eb154fa7bd5a2ae8d1d42d
