#! /bin/sh

set -e

: ${AWK=awk}

if [ $# -eq 1 ]; then
  top_srcdir="$1";
else
  top_srcdir="../.."
fi

dld_dir=$top_srcdir/src/DLD-FUNCTIONS

$AWK -f $dld_dir/config-module.awk < $dld_dir/module-files > $dld_dir/module.mk-t

$top_srcdir/move-if-change $dld_dir/module.mk-t $dld_dir/module.mk
