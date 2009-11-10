#! /bin/sh

set -e

: ${AWK=awk}

$AWK -f config-module.awk < module-files > module.mk-t

../../move-if-change module.mk-t module.mk
