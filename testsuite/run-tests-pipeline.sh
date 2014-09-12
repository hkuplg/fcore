#!/usr/bin/env bash

cd tests/pipeline

for m in 'naive' 'applyopt' 'stack'; do
  for f in $(ls *.sf); do
    f2j -r -m=$m $f
  done
done