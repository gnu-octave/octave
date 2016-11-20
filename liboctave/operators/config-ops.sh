#! /bin/sh

## Copyright (C) 2009-2016 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

set -e

AWK=${AWK:-awk}
SHELL=${SHELL:-/bin/sh}

if [ $# -lt 3 ]; then
  src_type="all"
else
  src_type="$3"
fi

if [ $# -lt 2 ]; then
  obj_type="all"
else
  obj_type="$2"
fi

if [ $# -lt 1 ]; then
  top_srcdir="../.."
else
  top_srcdir="$1"
fi

move_if_change="$top_srcdir/build-aux/move-if-change"

liboctave_dir="$top_srcdir/liboctave/operators"

mk_ops="$liboctave_dir/mk-ops.awk"

case "$obj_type" in
  vx | all)
    case "$src_type" in
      inc | all)
        VX_INC=$($AWK -f $mk_ops -v prefix=vx -v list_h_files=1 $liboctave_dir/vx-ops)
        echo "VX_OP_INC = $VX_INC" > $liboctave_dir/vx-op-inc.mk-t
        $SHELL $move_if_change $liboctave_dir/vx-op-inc.mk-t $liboctave_dir/vx-op-inc.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  vx | all)
    case "$src_type" in
      src | all)
        VX_SRC=$($AWK -f $mk_ops -v prefix=vx -v list_cc_files=1 $liboctave_dir/vx-ops)
        echo "VX_OP_SRC = $VX_SRC" > $liboctave_dir/vx-op-src.mk-t
        $SHELL $move_if_change $liboctave_dir/vx-op-src.mk-t $liboctave_dir/vx-op-src.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  mx | all)
    case "$src_type" in
      inc | all)
        MX_INC=$($AWK -f $mk_ops -v prefix=mx -v list_h_files=1 $liboctave_dir/mx-ops)
        echo "MX_OP_INC = $MX_INC" > $liboctave_dir/mx-op-inc.mk-t
        $SHELL $move_if_change $liboctave_dir/mx-op-inc.mk-t $liboctave_dir/mx-op-inc.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  mx | all)
    case "$src_type" in
      src | all)
        MX_SRC=$($AWK -f $mk_ops -v prefix=mx -v list_cc_files=1 $liboctave_dir/mx-ops)
        echo "MX_OP_SRC = $MX_SRC" > $liboctave_dir/mx-op-src.mk-t
        $SHELL $move_if_change $liboctave_dir/mx-op-src.mk-t $liboctave_dir/mx-op-src.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  smx | all)
    case "$src_type" in
      inc | all)
        SMX_INC=$($AWK -f $mk_ops -v prefix=smx -v list_h_files=1 $liboctave_dir/smx-ops)
        echo "SMX_OP_INC = $SMX_INC" > $liboctave_dir/smx-op-inc.mk-t
        $SHELL $move_if_change $liboctave_dir/smx-op-inc.mk-t $liboctave_dir/smx-op-inc.mk
      ;;
    esac
  ;;
esac

case "$obj_type" in
  smx | all)
    case "$src_type" in
      src | all)
        SMX_SRC=$($AWK -f $mk_ops -v prefix=smx -v list_cc_files=1 $liboctave_dir/smx-ops)
        echo "SMX_OP_SRC = $SMX_SRC" > $liboctave_dir/smx-op-src.mk-t
        $SHELL $move_if_change $liboctave_dir/smx-op-src.mk-t $liboctave_dir/smx-op-src.mk
      ;;
    esac
  ;;
esac
