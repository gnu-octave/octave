#! /bin/sh

if [ $# -ne 1 ]; then
  echo "usage: find-docstring-files TOP-SRCDIR" 1>&2
  exit 1
fi

## if there is a file in teh build directory tree, assume it is
## the file we are looking for.  Otherwise, get the one from the
## source tree.

if [ -f "../../scripts/DOCSTRINGS" ]; then
  echo "../../scripts/DOCSTRINGS"
else
  echo "$1/scripts/DOCSTRINGS"
fi

if [ -f "../../src/DOCSTRINGS" ]; then
  echo "../../src/DOCSTRINGS"
else
  echo "$1/src/DOCSTRINGS"
fi
