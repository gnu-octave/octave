#! /bin/sh
#
# Copyright (C) 2016 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

: ${SED=@SED@}

## Use two steps so that we can interpolate values without having to
## determine the order in which to set variable values.

## These must use ' so that embedded variables are not interpolated
## (the values they reference may not be defined before they are used).

AWK="@AWK@"
FIND="@FIND@"
SED="@SED@"
ADDRESS_SANITIZER_OPTIONS="@ADDRESS_SANITIZER_OPTIONS@"
abs_top_srcdir="@abs_top_srcdir@"
abs_top_builddir="@abs_top_builddir@"

## These must use " so that embedded variables are interpolated.

## Is there a better way?

function expand_var ()
{
  eval tmp="\$$1"
  while echo "$tmp" | grep '\${[A-Za-z_][A-Za-z0-9_]*}' > /dev/null; do
    eval tmp="$tmp"
  done
  eval $1="\"$tmp\""
}

expand_var AWK
expand_var FIND
expand_var SED
expand_var ADDRESS_SANITIZER_OPTIONS
expand_var abs_top_srcdir
expand_var abs_top_builddir

$SED \
  -e "s|%AWK%|${AWK}|g" \
  -e "s|%FIND%|${FIND}|g" \
  -e "s|%SED%|${SED}|g" \
  -e "s|%ADDRESS_SANITIZER_OPTIONS%|${ADDRESS_SANITIZER_OPTIONS}|g" \
  -e "s|%abs_top_srcdir%|${abs_top_srcdir}|" \
  -e "s|%builddir%|${abs_top_builddir}|"
