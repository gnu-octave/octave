#! /bin/sh

set -e

GREP=${GREP:-grep}
SED=${SED:-sed}

srcdir="$1"
if [ "$1" ]; then
  shift
fi

for arg
do
  if [ -f "$arg" ]; then
    file="$arg"
  else
    file="$srcdir/$arg"
  fi
  if [ -f "$file" ]; then
    if [ "`$GREP -l '^%!' $file`" ]; then
      echo "$file" | $SED "s,\\$srcdir/,,"
    fi
  fi
done
