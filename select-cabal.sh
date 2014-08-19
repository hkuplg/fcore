#!/usr/bin/env bash

set -e

project_name="systemfcompiler"

ghc --version

if [[ `ghc --version` =~ 7.8 ]]; then
  cp $project_name.cabal.ghc-7.8.3 $project_name.cabal
else
  cp $project_name.cabal.ghc-7.6.3 $project_name.cabal
fi
