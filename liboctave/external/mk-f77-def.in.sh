#! /bin/sh

########################################################################
##
## Copyright (C) 2006-2026 The Octave Project Developers
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

################################################################################
## Usage: mk-f77-def.sh SRCDIR file1.f [file2.f]...
## Purpose: Find function symbols to export from Fortran files.
################################################################################

set -e

if [ $# -lt 2 ]; then
  echo "usage: mk-f77-def.sh SRCDIR file1.f [file2.f]..." 1>&2
  exit 1
fi

srcdir="$1"
shift

: ${SED=@SED@}
: ${AWK=@AWK@}

F77_TOLOWER="@F77_TOLOWER@"
F77_APPEND_UNDERSCORE="@F77_APPEND_UNDERSCORE@"
F77_APPEND_EXTRA_UNDERSCORE="@F77_APPEND_EXTRA_UNDERSCORE@"

if test $F77_TOLOWER = yes; then
  case_cmd=tolower
else
  case_cmd=toupper
fi

if test $F77_APPEND_UNDERSCORE = yes; then
  uscore=_
else
  uscore=
fi

if test $F77_APPEND_EXTRA_UNDERSCORE = yes; then
  awkcmd="$AWK '{ if (\$0 ~ /_/) extra = \"_\"; else extra = \"\"; printf (\"%s%s%s\n\", $case_cmd (\$0), \"$uscore\", extra); }'"
else
  awkcmd="$AWK '{ printf (\"%s%s\n\", tolower (\$0), \"$uscore\"); }'"
fi

echo EXPORTS
for arg
do
  case "$arg" in
    *.f)
      ## There are TABS in this sed command.
      $SED -n -e 'y/ABCDEFGHIJLKMNOPQRSTUVWXYZ/abcdefghijlkmnopqrstuvwxyz/; s/^\(      \|	\)[ 	]*\(.*function\|subroutine\|entry\)[ 	]*\([^ 	(]*\).*$/\3/p' "$srcdir/$arg" | eval $awkcmd
    ;;
  esac
done
