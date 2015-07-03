#! /bin/sh

if [ $# -ne 1 ]; then
  echo "usage: find-docstring-files TOP-SRCDIR" 1>&2
  exit 1
fi

## Look in the build tree then the source tree for the
## DOCSTRINGS files.  Fail if neither exists.

if [ -f "scripts/DOCSTRINGS" ]; then
  echo "scripts/DOCSTRINGS"
elif [ -f "$1/scripts/DOCSTRINGS" ]; then
  echo "$1/scripts/DOCSTRINGS"
else
  echo "find-docstring-files: scripts/DOCSTRINGS file is missing!" 1>&2
  exit 1
fi

if [ -f "libinterp/DOCSTRINGS" ]; then
  echo "libinterp/DOCSTRINGS"
elif [ -f "$1/libinterp/DOCSTRINGS" ]; then
  echo "$1/libinterp/DOCSTRINGS"
else
  echo "find-docstring-files: libinterp/DOCSTRINGS file is missing!" 1>&2
  exit 1
fi
