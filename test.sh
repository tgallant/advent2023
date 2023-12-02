#!/bin/bash

cd "src/$1"
emacs -batch -l ../../aoc.el -l $1.el -f ert-run-tests-batch-and-exit
STATUS=$?
cd -
exit $STATUS
