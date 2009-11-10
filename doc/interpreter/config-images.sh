#! /bin/sh

set -e

: ${AWK=awk}

$AWK -f images.awk images > images.mk-t
../../move-if-change images.mk-t images.mk
