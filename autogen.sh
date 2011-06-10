#! /bin/sh
# autogen.sh
# Run this to generate all the initial makefiles, etc.

set -e

## Use --foreign since we auto-generate the AUTHORS file and the default
## --gnu strictness level doesn't like it if the AUTHORS file is missing.

AUTOMAKE="automake --foreign --warnings=no-portability"
export AUTOMAKE

## Check for files that automake --gnu would normally look for, except
## AUTHORS, which we autogenerate from the documentation files along with
## building the rest of Octave, and INSTALL, which is linked from
## gnulib/doc/INSTALL by the bootstrap script.

for f in NEWS README COPYING; do
  if ! test -f $f; then
    echo "required file $f is missing" 2>&1
    exit 1
  fi
done

echo "generating source lists for liboctave/Makefile..."

(cd liboctave; ./config-ops.sh)

echo "generating doc/interpreter/images.mk..."

(cd doc/interpreter; ./config-images.sh)

echo "generating src/DLD-FUNCTIONS/module.mk..."

(cd src/DLD-FUNCTIONS; ./config-module.sh)

echo "bootstrapping..."

build-aux/bootstrap "$@"
