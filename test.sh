#!/bin/bash

cd "lisp"
emacs -batch -l aoc2023.el -l $1.el -f ert-run-tests-batch-and-exit
STATUS=$?
cd -
exit $STATUS
