#! /bin/sh
# autogen.sh
# Run this to generate all the initial makefiles, etc.

set -e

AUTOMAKE="automake --warnings=no-portability"

echo "generating source lists for liboctave/Makefile..."

(cd liboctave; ./config-ops.sh)

echo "generating doc/interpreter/images.mk..."

(cd doc/interpreter; ./config-images.sh)

echo "generating src/DLD-FUNCTIONS/module.mk..."

(cd src/DLD-FUNCTIONS; ./config-module.sh)

echo "bootstrapping..."

./bootstrap "$@"
