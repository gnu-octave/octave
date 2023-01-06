#! /bin/sh

########################################################################
##
## Copyright (C) 2009-2023 The Octave Project Developers
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

set -e

SED=${SED:-sed}
EGREP=${EGREP:-egrep}

# Some stupid egreps don't like empty elements in alternation patterns,
# so we have to repeat ourselves because some stupid egreps don't like
# empty elements in alternation patterns.

DEFUN_PATTERN="^[ \t]*DEF(CONSTFUN|CONSTMETHOD|METHOD|METHOD(_|_STATIC_)DLD|METHODX|METHODX(_|_STATIC_)DLD|UN|UN(_|_STATIC_)DLD|UNX|UNX(_|_STATIC_)DLD)[ \t]*\\("

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
    if [ "`$EGREP -l "$DEFUN_PATTERN" $file`" ]; then
      echo "$file" | $SED "s,\\$srcdir/,,"
    fi
  fi
done
