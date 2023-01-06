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

AWK=${AWK:-awk}
SHELL=${SHELL:-/bin/sh}

if [ $# -eq 1 ]; then
  top_srcdir="$1"
else
  top_srcdir="../.."
fi

move_if_change="$top_srcdir/build-aux/move-if-change"

interp_dir=$top_srcdir/doc/interpreter

$AWK -v dir=doc/interpreter -f $interp_dir/images.awk < $interp_dir/images > $interp_dir/images.mk-t

$SHELL $move_if_change $interp_dir/images.mk-t $interp_dir/images.mk
