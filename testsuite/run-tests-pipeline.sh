#!/usr/bin/env bash

cd tests/pipeline

for f in `ls *.sf`; do
  f2j -r $f
done