#! /bin/sh

########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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

# Generate a header file that provides the public symbols from Octave's
# autoconf-generated config.h file.  See the notes at the top of the
# generated octave-config.h file for more details.

set -e

if [ $# -ne 1 ] && [ $# -ne 2 ]; then
  echo "usage: mk-hg-id.sh SRCDIR [--disable]" 1>&2
  exit 1
fi

srcdir="$1"

hg_id=HG-ID
move_if_change="$srcdir/build-aux/move-if-change"

## A user's ~/.hgrc may redefine or add default options to any hg subcommand,
## potentially altering its behavior and possibly its standard output.  Always
## run hg subcommands with configuration variables set to ensure that the
## user's preferences do not influence the expected behavior.
hg_safe ()
{
  cmd=$1; shift
  hg --config alias.${cmd}=${cmd} --config defaults.${cmd}= ${cmd} "$@"
}

if [ $# -eq 2 ] && [ x"$2" = x--disable ]; then
  echo "hg-id-disabled" > ${hg_id}-t
  ${move_if_change} ${hg_id}-t ${hg_id}
elif [ -d $srcdir/.hg ]; then
  ( cd $srcdir && hg_safe identify --id || echo "unknown" ) > ${hg_id}-t
  ${move_if_change} ${hg_id}-t ${hg_id}
elif [ ! -f $srcdir/${hg_id} ]; then
  echo "WARNING: $srcdir/${hg_id} is missing!" 1>&2
  echo "unknown" > ${hg_id}-t && mv ${hg_id}-t ${hg_id}
else
  echo "preserving existing ${hg_id} file" 1>&2
  if [ "x$srcdir" != "x." ] && [ -f $srcdir/${hg_id} ] && [ ! -f ${hg_id} ]; then
    cp ${srcdir}/${hg_id} ${hg_id}
    touch -r ${srcdir}/${hg_id} ${hg_id}
  fi
fi

if [ "`cat ${hg_id}`" = "hg-id-disabled" ]; then
  echo "WARNING: ${hg_id} is 'hg-id-disabled'" 1>&2
fi

cat ${hg_id}
