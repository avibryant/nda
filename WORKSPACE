workspace(name = "ad")

git_repository(
    name = "io_bazel_rules_scala",
    remote = "git://github.com/bazelbuild/rules_scala",
    commit = "0113d17656df43c4252cfd34c1feade2c4370a63"
)

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()
load("@io_bazel_rules_scala//tut_rule:tut.bzl", "tut_repositories")
tut_repositories()
