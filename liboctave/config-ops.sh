#! /bin/sh

set -e

: ${AWK=awk}

VX_INC=$($AWK -f mk-ops.awk prefix=vx list_h_files=1 vx-ops)

MX_INC=$($AWK -f mk-ops.awk prefix=mx list_h_files=1 mx-ops)

SMX_INC=$($AWK -f sparse-mk-ops.awk prefix=smx list_h_files=1 sparse-mx-ops)

VX_SRC=$($AWK -f mk-ops.awk prefix=vx list_cc_files=1 vx-ops)

MX_SRC=$($AWK -f mk-ops.awk prefix=mx list_cc_files=1 mx-ops)

SMX_SRC=$($AWK -f sparse-mk-ops.awk prefix=smx list_cc_files=1 sparse-mx-ops)

echo "VX_OP_INC = $(echo $VX_INC)" > vx-op-inc.mk-t
../move-if-change vx-op-inc.mk-t vx-op-inc.mk

echo "MX_OP_INC = $(echo $MX_INC)" > mx-op-inc.mk-t
../move-if-change mx-op-inc.mk-t mx-op-inc.mk

echo "SMX_OP_INC = $(echo $SMX_INC)" > smx-op-inc.mk-t
../move-if-change smx-op-inc.mk-t smx-op-inc.mk

echo "VX_OP_SRC = $(echo $VX_SRC)" > vx-op-src.mk-t
../move-if-change vx-op-src.mk-t vx-op-src.mk

echo "MX_OP_SRC = $(echo $MX_SRC)" > mx-op-src.mk-t
../move-if-change mx-op-src.mk-t mx-op-src.mk

echo "SMX_OP_SRC = $(echo $SMX_SRC)" > smx-op-src.mk-t
../move-if-change smx-op-src.mk-t smx-op-src.mk
