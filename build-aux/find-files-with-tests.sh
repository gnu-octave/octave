#! /bin/sh

set -e
SED=${SED:-sed}

srcdir="$1"
shift

for arg
do
  if [ -f "$arg" ]; then
    file="$arg"
  else
    file="$srcdir/$arg"
  fi
  if [ "`grep -l '^%!' $file`" ]; then
    echo "$file" | $SED "s,\\$srcdir/,,";
  fi
done
