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

clean_varname="liboctave_MAINTAINERCLEANFILES"

liboctave_dir="$top_srcdir/liboctave/operators"

mk_ops="$liboctave_dir/mk-ops.awk"

case "$obj_type" in
  vx | all)
    case "$src_type" in
      inc | all)
        fn=vx-op-inc.mk
        VX_INC=$($AWK -f $mk_ops -v prefix=vx -v list_h_files=1 $liboctave_dir/vx-ops)
        echo "VX_OP_INC = $VX_INC" > $liboctave_dir/$fn-t
        echo "$clean_varname += \$(srcdir)/%reldir%/$fn" >> $liboctave_dir/$fn-t
        $SHELL $move_if_change $liboctave_dir/$fn-t $liboctave_dir/$fn
      ;;
    esac
  ;;
esac

case "$obj_type" in
  vx | all)
    case "$src_type" in
      src | all)
        fn=vx-op-src.mk
        VX_SRC=$($AWK -f $mk_ops -v prefix=vx -v list_cc_files=1 $liboctave_dir/vx-ops)
        echo "VX_OP_SRC = $VX_SRC" > $liboctave_dir/$fn-t
        echo "$clean_varname += \$(srcdir)/%reldir%/$fn" >> $liboctave_dir/$fn-t
        $SHELL $move_if_change $liboctave_dir/$fn-t $liboctave_dir/$fn
      ;;
    esac
  ;;
esac

case "$obj_type" in
  mx | all)
    case "$src_type" in
      inc | all)
        fn=mx-op-inc.mk
        MX_INC=$($AWK -f $mk_ops -v prefix=mx -v list_h_files=1 $liboctave_dir/mx-ops)
        echo "MX_OP_INC = $MX_INC" > $liboctave_dir/$fn-t
        echo "$clean_varname += \$(srcdir)/%reldir%/$fn" >> $liboctave_dir/$fn-t
        $SHELL $move_if_change $liboctave_dir/$fn-t $liboctave_dir/$fn
      ;;
    esac
  ;;
esac

case "$obj_type" in
  mx | all)
    case "$src_type" in
      src | all)
        fn=mx-op-src.mk
        MX_SRC=$($AWK -f $mk_ops -v prefix=mx -v list_cc_files=1 $liboctave_dir/mx-ops)
        echo "MX_OP_SRC = $MX_SRC" > $liboctave_dir/$fn-t
        echo "$clean_varname += \$(srcdir)/%reldir%/$fn" >> $liboctave_dir/$fn-t
        $SHELL $move_if_change $liboctave_dir/$fn-t $liboctave_dir/$fn
      ;;
    esac
  ;;
esac

case "$obj_type" in
  smx | all)
    case "$src_type" in
      inc | all)
        fn=smx-op-inc.mk
        SMX_INC=$($AWK -f $mk_ops -v prefix=smx -v list_h_files=1 $liboctave_dir/smx-ops)
        echo "SMX_OP_INC = $SMX_INC" > $liboctave_dir/$fn-t
        echo "$clean_varname += \$(srcdir)/%reldir%/$fn" >> $liboctave_dir/$fn-t
        $SHELL $move_if_change $liboctave_dir/$fn-t $liboctave_dir/$fn
      ;;
    esac
  ;;
esac

case "$obj_type" in
  smx | all)
    case "$src_type" in
      src | all)
        fn=smx-op-src.mk
        SMX_SRC=$($AWK -f $mk_ops -v prefix=smx -v list_cc_files=1 $liboctave_dir/smx-ops)
        echo "SMX_OP_SRC = $SMX_SRC" > $liboctave_dir/$fn-t
        echo "$clean_varname += \$(srcdir)/%reldir%/$fn" >> $liboctave_dir/$fn-t
        $SHELL $move_if_change $liboctave_dir/$fn-t $liboctave_dir/$fn
      ;;
    esac
  ;;
esac
