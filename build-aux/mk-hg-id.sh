#! /bin/sh

########################################################################
##
## Copyright (C) 2016-2026 The Octave Project Developers
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
## Usage: mk-hg-id.sh SRCDIR
## Purpose: Display the Mercurial ID of the current revision.
## If the source tree does not appear to be a Mercurial repo but does contain a
## file named HG-ID, display the contents of that file.  If that file does not
## exist, or the Mercurial fails to provide the ID, display "unknown-hg-id".
################################################################################

set -e

if [ $# -ne 1 ]; then
  echo "usage: mk-hg-id.sh SRCDIR" 1>&2
  exit 1
fi

srcdir="$1"

hg_id=HG-ID

## A user's ~/.hgrc may redefine or add default options to any hg subcommand,
## potentially altering its behavior and possibly its standard output.  Always
## run hg subcommands with configuration variables set to ensure that the
## user's preferences do not influence the expected behavior.

hg_safe ()
{
  cmd=$1; shift
  hg --config alias.${cmd}=${cmd} --config defaults.${cmd}= ${cmd} "$@"
}

if [ -d "$srcdir/.hg" ]; then
  ( cd "$srcdir" && hg_safe identify --id || echo "unknown-hg-id" )
elif [ -d "$srcdir/.git" ]; then
  ( cd "$srcdir" && echo "git:"$(git rev-parse --short HEAD) || echo "unknown-hg-id" )
elif [ -f "$srcdir/$hg_id" ]; then
  cat "$srcdir/$hg_id"
else
  echo "WARNING: $srcdir/$hg_id is missing!" 1>&2
  echo "unknown-hg-id"
fi
