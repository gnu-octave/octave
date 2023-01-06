#! /bin/sh

########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## Attempt to get traditional sort behavior based on byte values.
LC_ALL=C
export LC_ALL

set -e

SED=${SED:-sed}

srcdir="$1"
shift

for arg
do
  src_file="$srcdir/$arg"

  if [ -f "$src_file" ]; then

    ## Compute and print the autoloads.

    base=`basename "$src_file" | $SED 's/\.cc$//'`
    fcns=`$SED -n \
      -e 's/^ *DEF\(METHOD\|UN\)_\(\|STATIC_\)DLD *( *\([^, ]*\) *,.*$/\3/p' \
      -e 's/^ *DEF\(METHOD\|UN\)X_\(\|STATIC_\)DLD *( *"\([^"]*\)".*$/\3/p' \
      "$src_file" | \
      sort -u`
    if [ -n "$fcns" ]; then
      for n in $fcns; do
        if [ "$n" = "$base" ]; then
          true
        else
          echo "autoload (\"$n\", \"$base.oct\");"
        fi
      done
    fi

    ## Process PKG_ADD directives after autoloads so that all
    ## necessary functions can be found before they are used.

    $SED -n -e 's,^//* *PKG_ADD: *,,p' \
            -e 's,^/\* *PKG_ADD: *\(.*\) *\*/ *$,\1,p' "$src_file"

  fi
done

exit $?
