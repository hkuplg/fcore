#!/usr/bin/env bash

# Run a particular set of specs.
# SYNOPSIS
#       ./run-spec.sh PATH ...

set -e

srcdir=compiler
testdir=testsuite

for spec in $@; do
  runhaskell -i$srcdir:$testdir $spec
done
