#! /bin/sh

set -e

: ${AWK=awk}

if [ $# -eq 1 ]; then
  top_srcdir="$1";
else
  top_srcdir="../.."
fi

interp_dir=$top_srcdir/doc/interpreter

$AWK -f $interp_dir/images.awk < $interp_dir/images > $interp_dir/images.mk-t

$top_srcdir/move-if-change $interp_dir/images.mk-t $interp_dir/images.mk
