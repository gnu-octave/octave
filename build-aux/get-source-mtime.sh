#! /bin/sh

########################################################################
##
## Copyright (C) 2017-2026 The Octave Project Developers
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
## USAGE: get-source-mtime.sh SRCDIR
## PURPOSE: Generate a timestamp that best represents the last modification
## time of this source tree.  The time value is printed on stdout in units of
## time_t.  If a reasonable representation of the source tree last modification
## time can not be determined, then the current system time is printed instead.
## A valid time_t value is always printed on stdout.
################################################################################

set -e

if [ $# -ne 1 ]; then
  echo 1>&2 "usage: get-source-mtime.sh SRCDIR"
  exit 1
fi

PERL=${PERL:-perl}
SED=${SED:-sed}

srcdir="$1"

## A user's ~/.hgrc may redefine or add default options to any hg subcommand,
## potentially altering its behavior and possibly its standard output.  Always
## run hg subcommands with configuration variables set to ensure that the
## user's preferences do not influence the expected beavior.
hg_safe ()
{
  cmd=$1; shift
  hg --config alias.${cmd}=${cmd} --config defaults.${cmd}= ${cmd} "$@"
}

if [ -n "$SOURCE_DATE_EPOCH" ]; then
  ## Allow the source modification time to be overridden by SOURCE_DATE_EPOCH
  tm=$SOURCE_DATE_EPOCH
elif [ -d $srcdir/.hg ]; then
  tm=$( cd $srcdir && hg_safe log --rev . --template '{date|hgdate}' )
  tm=$( echo $tm | $SED -n 's/^\([0-9]\+\) .*/\1/p' )
elif [ -f $srcdir/HG-ID ]; then
  tm=$( $PERL -e '@s = stat($ARGV[0]); print($s[9]) if @s;' $srcdir/HG-ID )
elif [ -f $srcdir/configure ]; then
  tm=$( $PERL -e '@s = stat($ARGV[0]); print($s[9]) if @s;' $srcdir/configure )
fi

if [ -z "$tm" ]; then
  tm=$( date +%s )
fi

echo $tm
