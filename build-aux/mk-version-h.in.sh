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

OCTAVE_API_VERSION='@OCTAVE_API_VERSION@'
OCTAVE_COPYRIGHT='@OCTAVE_COPYRIGHT@'
OCTAVE_MAJOR_VERSION='@OCTAVE_MAJOR_VERSION@'
OCTAVE_MINOR_VERSION='@OCTAVE_MINOR_VERSION@'
OCTAVE_PATCH_VERSION='@OCTAVE_PATCH_VERSION@'
OCTAVE_RELEASE_DATE='@OCTAVE_RELEASE_DATE@'
OCTAVE_VERSION='@OCTAVE_VERSION@'

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

expand_var OCTAVE_API_VERSION
expand_var OCTAVE_COPYRIGHT
expand_var OCTAVE_MAJOR_VERSION
expand_var OCTAVE_MINOR_VERSION
expand_var OCTAVE_PATCH_VERSION
expand_var OCTAVE_RELEASE_DATE
expand_var OCTAVE_VERSION

$SED \
  -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically by mk-version-h.|" \
  -e "s|%OCTAVE_API_VERSION%|\"${OCTAVE_API_VERSION}\"|" \
  -e "s|%OCTAVE_COPYRIGHT%|\"${OCTAVE_COPYRIGHT}\"|" \
  -e "s|%OCTAVE_MAJOR_VERSION%|${OCTAVE_MAJOR_VERSION}|" \
  -e "s|%OCTAVE_MINOR_VERSION%|${OCTAVE_MINOR_VERSION}|" \
  -e "s|%OCTAVE_PATCH_VERSION%|${OCTAVE_PATCH_VERSION}|" \
  -e "s|%OCTAVE_RELEASE_DATE%|\"${OCTAVE_RELEASE_DATE}\"|" \
  -e "s|%OCTAVE_VERSION%|\"${OCTAVE_VERSION}\"|"
