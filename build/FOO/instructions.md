# Instructions for the base layer:

Please file issues at https://github.com/jin/create-bazel-workspace

# Instructions for the go layer:

From rules_go, please run:

    $ bazel run //:gazelle

This will generate a BUILD.bazel file for each Go package in your repository.
You can run the same command in the future to update existing build files with
new source files, dependencies, and options.

Please file issues at https://github.com/bazelbuild/rules_go

# Instructions for the scala layer:

Please file issues at https://github.com/bazelbuild/rules_scala

