#! /bin/sh

set -e

AWK=${AWK:-awk}
SHELL=${SHELL:-/bin/sh}

if [ $# -eq 1 ]; then
  top_srcdir="$1"
else
  top_srcdir="../.."
fi

move_if_change="$top_srcdir/build-aux/move-if-change"

dld_dir=$top_srcdir/libinterp/dldfcn

$AWK -f $dld_dir/config-module.awk < $dld_dir/module-files > $dld_dir/module.mk-t

$SHELL $move_if_change $dld_dir/module.mk-t $dld_dir/module.mk
