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
    - Full build
    - [Manual testing][3]
1. If all tests pass, tag the commit with the release version.
1. Perform [manual testing][3] that needs the tagging (source maps).
1. Publish:
    - Sonatype, bintray (`./script/publish.sh`)
    - Docs to website: Use
      `~/fetchapis.sh <full sjs version> <binary sjs version>` on the webserver
      once artifacts are on maven central.
1. Once artifacts are on maven central, create a "Towards x.y.z." commit
   ([example][5]).
    1. Create an "FF ONLY" PR for CI and review.
    1. Once you have LGTM, push the commit (do *not* click the merge button)
1. Prepare release announcement, taking the last one as model ([example][6]).
1. When merging the release announcement PR (after proper review):
    - Update the latest/ URLs (use `~/setlatestapi.sh <full sjs version>` on
      webserver)
    - Announce on Twitter using the @scala_js account
    - Announce on [Gitter](https://gitter.im/scala-js/scala-js)
    - Cross-post as an Announcement in Scala Users ([example][7])

[1]: https://github.com/scala-js/scala-js/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20no%3Amilestone%20-label%3Ainvalid%20-label%3Aduplicate%20-label%3Aas-designed%20-label%3Aquestion%20-label%3Awontfix%20-label%3A%22can%27t%20reproduce%22%20-label%3A%22separate%20repo%22
[2]: https://github.com/scala-js/scala-js/commit/c3520bb9dae46757a975cccd428a77b8d6e6a75e
[3]: https://github.com/scala-js/scala-js/blob/master/TESTING.md
[5]: https://github.com/scala-js/scala-js/commit/c6c82e80f56bd2008ff8273088bbbbbbbc30f777
[6]: https://github.com/scala-js/scala-js-website/commit/057f743c3fb8abe6077fb4debeeec45cd5c53d5d
[7]: https://users.scala-lang.org/t/announcing-scala-js-1-4-0/7013
