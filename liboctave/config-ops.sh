#! /bin/sh

set -e

: ${AWK=awk}

if [ $# -eq 1 ]; then
  top_srcdir="$1";
else
  top_srcdir=".."
fi

liboctave_dir="$top_srcdir/liboctave"

mk_ops="$liboctave_dir/mk-ops.awk"
sparse_mk_ops="$liboctave_dir/sparse-mk-ops.awk"

VX_INC=$($AWK -f $mk_ops prefix=vx list_h_files=1 $liboctave_dir/vx-ops)

MX_INC=$($AWK -f $mk_ops prefix=mx list_h_files=1 $liboctave_dir/mx-ops)

SMX_INC=$($AWK -f $sparse_mk_ops prefix=smx list_h_files=1 $liboctave_dir/sparse-mx-ops)

VX_SRC=$($AWK -f $mk_ops prefix=vx list_cc_files=1 $liboctave_dir/vx-ops)

MX_SRC=$($AWK -f $mk_ops prefix=mx list_cc_files=1 $liboctave_dir/mx-ops)

SMX_SRC=$($AWK -f $sparse_mk_ops prefix=smx list_cc_files=1 $liboctave_dir/sparse-mx-ops)

echo "VX_OP_INC = $(echo $VX_INC)" > $liboctave_dir/vx-op-inc.mk-t
$top_srcdir/move-if-change $liboctave_dir/vx-op-inc.mk-t $liboctave_dir/vx-op-inc.mk

echo "MX_OP_INC = $(echo $MX_INC)" > $liboctave_dir/mx-op-inc.mk-t
$top_srcdir/move-if-change $liboctave_dir/mx-op-inc.mk-t $liboctave_dir/mx-op-inc.mk

echo "SMX_OP_INC = $(echo $SMX_INC)" > $liboctave_dir/smx-op-inc.mk-t
$top_srcdir/move-if-change $liboctave_dir/smx-op-inc.mk-t $liboctave_dir/smx-op-inc.mk

echo "VX_OP_SRC = $(echo $VX_SRC)" > $liboctave_dir/vx-op-src.mk-t
$top_srcdir/move-if-change $liboctave_dir/vx-op-src.mk-t $liboctave_dir/vx-op-src.mk

echo "MX_OP_SRC = $(echo $MX_SRC)" > $liboctave_dir/mx-op-src.mk-t
$top_srcdir/move-if-change $liboctave_dir/mx-op-src.mk-t $liboctave_dir/mx-op-src.mk

echo "SMX_OP_SRC = $(echo $SMX_SRC)" > $liboctave_dir/smx-op-src.mk-t
$top_srcdir/move-if-change $liboctave_dir/smx-op-src.mk-t $liboctave_dir/smx-op-src.mk
