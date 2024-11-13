#!/bin/bash

cd "lisp"
emacs -batch -l aoc2023.el -l $1 -f ert-run-tests-batch-and-exit
STATUS=$?
cd -
exit $STATUS
