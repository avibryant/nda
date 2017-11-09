scala_library(
    name = "examples",
    srcs = glob(["*.scala"]),
    visibility = ["//visibility:public"],
    deps = ["//src/core", "//src/session", "//src/gradient"]
)

scala_binary(
    name = "regression",
    deps = [":examples"],
    main_class ="nda.Regression"
)