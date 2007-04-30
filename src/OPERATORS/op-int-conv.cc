/*

Copyright (C) 2004 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-range.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-str-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"

// conversion ops

DEFCONVFN (scalar_to_int8, scalar, int8)
DEFCONVFN (scalar_to_int16, scalar, int16)
DEFCONVFN (scalar_to_int32, scalar, int32)
DEFCONVFN (scalar_to_int64, scalar, int64)

DEFCONVFN (scalar_to_uint8, scalar, uint8)
DEFCONVFN (scalar_to_uint16, scalar, uint16)
DEFCONVFN (scalar_to_uint32, scalar, uint32)
DEFCONVFN (scalar_to_uint64, scalar, uint64)

DEFCONVFN (matrix_to_int8, matrix, int8)
DEFCONVFN (matrix_to_int16, matrix, int16)
DEFCONVFN (matrix_to_int32, matrix, int32)
DEFCONVFN (matrix_to_int64, matrix, int64)

DEFCONVFN (matrix_to_uint8, matrix, uint8)
DEFCONVFN (matrix_to_uint16, matrix, uint16)
DEFCONVFN (matrix_to_uint32, matrix, uint32)
DEFCONVFN (matrix_to_uint64, matrix, uint64)

DEFCONVFN (bool_to_int8, bool, int8)
DEFCONVFN (bool_to_int16, bool, int16)
DEFCONVFN (bool_to_int32, bool, int32)
DEFCONVFN (bool_to_int64, bool, int64)

DEFCONVFN (bool_to_uint8, bool, uint8)
DEFCONVFN (bool_to_uint16, bool, uint16)
DEFCONVFN (bool_to_uint32, bool, uint32)
DEFCONVFN (bool_to_uint64, bool, uint64)

DEFCONVFN (bool_matrix_to_int8, bool_matrix, int8)
DEFCONVFN (bool_matrix_to_int16, bool_matrix, int16)
DEFCONVFN (bool_matrix_to_int32, bool_matrix, int32)
DEFCONVFN (bool_matrix_to_int64, bool_matrix, int64)

DEFCONVFN (bool_matrix_to_uint8, bool_matrix, uint8)
DEFCONVFN (bool_matrix_to_uint16, bool_matrix, uint16)
DEFCONVFN (bool_matrix_to_uint32, bool_matrix, uint32)
DEFCONVFN (bool_matrix_to_uint64, bool_matrix, uint64)

DEFSTRINTCONVFN (char_matrix_sq_str_to_int8, int8)
DEFSTRINTCONVFN (char_matrix_sq_str_to_int16, int16)
DEFSTRINTCONVFN (char_matrix_sq_str_to_int32, int32)
DEFSTRINTCONVFN (char_matrix_sq_str_to_int64, int64)

DEFSTRINTCONVFN (char_matrix_sq_str_to_uint8, uint8)
DEFSTRINTCONVFN (char_matrix_sq_str_to_uint16, uint16)
DEFSTRINTCONVFN (char_matrix_sq_str_to_uint32, uint32)
DEFSTRINTCONVFN (char_matrix_sq_str_to_uint64, uint64)

DEFSTRINTCONVFN (char_matrix_dq_str_to_int8, int8)
DEFSTRINTCONVFN (char_matrix_dq_str_to_int16, int16)
DEFSTRINTCONVFN (char_matrix_dq_str_to_int32, int32)
DEFSTRINTCONVFN (char_matrix_dq_str_to_int64, int64)

DEFSTRINTCONVFN (char_matrix_dq_str_to_uint8, uint8)
DEFSTRINTCONVFN (char_matrix_dq_str_to_uint16, uint16)
DEFSTRINTCONVFN (char_matrix_dq_str_to_uint32, uint32)
DEFSTRINTCONVFN (char_matrix_dq_str_to_uint64, uint64)

DEFCONVFN (range_to_int8, range, int8)
DEFCONVFN (range_to_int16, range, int16)
DEFCONVFN (range_to_int32, range, int32)
DEFCONVFN (range_to_int64, range, int64)

DEFCONVFN (range_to_uint8, range, uint8)
DEFCONVFN (range_to_uint16, range, uint16)
DEFCONVFN (range_to_uint32, range, uint32)
DEFCONVFN (range_to_uint64, range, uint64)

#define INT_CONV_FUNCTIONS(tfrom) \
  DEFCONVFN2 (tfrom ## _scalar_to_int8, tfrom, scalar, int8) \
  DEFCONVFN2 (tfrom ## _scalar_to_int16, tfrom, scalar, int16) \
  DEFCONVFN2 (tfrom ## _scalar_to_int32, tfrom, scalar, int32) \
  DEFCONVFN2 (tfrom ## _scalar_to_int64, tfrom, scalar, int64) \
 \
  DEFCONVFN2 (tfrom ## _scalar_to_uint8, tfrom, scalar, uint8) \
  DEFCONVFN2 (tfrom ## _scalar_to_uint16, tfrom, scalar, uint16) \
  DEFCONVFN2 (tfrom ## _scalar_to_uint32, tfrom, scalar, uint32) \
  DEFCONVFN2 (tfrom ## _scalar_to_uint64, tfrom, scalar, uint64) \
 \
  DEFCONVFN2 (tfrom ## _matrix_to_int8, tfrom, matrix, int8) \
  DEFCONVFN2 (tfrom ## _matrix_to_int16, tfrom, matrix, int16) \
  DEFCONVFN2 (tfrom ## _matrix_to_int32, tfrom, matrix, int32) \
  DEFCONVFN2 (tfrom ## _matrix_to_int64, tfrom, matrix, int64) \
 \
  DEFCONVFN2 (tfrom ## _matrix_to_uint8, tfrom, matrix, uint8) \
  DEFCONVFN2 (tfrom ## _matrix_to_uint16, tfrom, matrix, uint16) \
  DEFCONVFN2 (tfrom ## _matrix_to_uint32, tfrom, matrix, uint32) \
  DEFCONVFN2 (tfrom ## _matrix_to_uint64, tfrom, matrix, uint64)

INT_CONV_FUNCTIONS (int8)
INT_CONV_FUNCTIONS (int16)
INT_CONV_FUNCTIONS (int32)
INT_CONV_FUNCTIONS (int64)

INT_CONV_FUNCTIONS (uint8)
INT_CONV_FUNCTIONS (uint16)
INT_CONV_FUNCTIONS (uint32)
INT_CONV_FUNCTIONS (uint64)

#define INSTALL_INT_CONV_FUNCTIONS(tfrom) \
  INSTALL_CONVOP (octave_ ## tfrom ## _scalar, octave_int8_matrix, tfrom ## _scalar_to_int8) \
  INSTALL_CONVOP (octave_ ## tfrom ## _scalar, octave_int16_matrix, tfrom ## _scalar_to_int16) \
  INSTALL_CONVOP (octave_ ## tfrom ## _scalar, octave_int32_matrix, tfrom ## _scalar_to_int32) \
  INSTALL_CONVOP (octave_ ## tfrom ## _scalar, octave_int64_matrix, tfrom ## _scalar_to_int64) \
 \
  INSTALL_CONVOP (octave_ ## tfrom ## _scalar, octave_uint8_matrix, tfrom ## _scalar_to_uint8) \
  INSTALL_CONVOP (octave_ ## tfrom ## _scalar, octave_uint16_matrix, tfrom ## _scalar_to_uint16) \
  INSTALL_CONVOP (octave_ ## tfrom ## _scalar, octave_uint32_matrix, tfrom ## _scalar_to_uint32) \
  INSTALL_CONVOP (octave_ ## tfrom ## _scalar, octave_uint64_matrix, tfrom ## _scalar_to_uint64) \
 \
  INSTALL_CONVOP (octave_ ## tfrom ## _matrix, octave_int8_matrix, tfrom ## _matrix_to_int8) \
  INSTALL_CONVOP (octave_ ## tfrom ## _matrix, octave_int16_matrix, tfrom ## _matrix_to_int16) \
  INSTALL_CONVOP (octave_ ## tfrom ## _matrix, octave_int32_matrix, tfrom ## _matrix_to_int32) \
  INSTALL_CONVOP (octave_ ## tfrom ## _matrix, octave_int64_matrix, tfrom ## _matrix_to_int64) \
 \
  INSTALL_CONVOP (octave_ ## tfrom ## _matrix, octave_uint8_matrix, tfrom ## _matrix_to_uint8) \
  INSTALL_CONVOP (octave_ ## tfrom ## _matrix, octave_uint16_matrix, tfrom ## _matrix_to_uint16) \
  INSTALL_CONVOP (octave_ ## tfrom ## _matrix, octave_uint32_matrix, tfrom ## _matrix_to_uint32) \
  INSTALL_CONVOP (octave_ ## tfrom ## _matrix, octave_uint64_matrix, tfrom ## _matrix_to_uint64)

#define INSTALL_CONVOPS(tfrom) \
  INSTALL_CONVOP (octave_ ## tfrom, octave_int8_matrix, tfrom ## _to_int8) \
  INSTALL_CONVOP (octave_ ## tfrom, octave_int16_matrix, tfrom ## _to_int16) \
  INSTALL_CONVOP (octave_ ## tfrom, octave_int32_matrix, tfrom ## _to_int32) \
  INSTALL_CONVOP (octave_ ## tfrom, octave_int64_matrix, tfrom ## _to_int64) \
 \
  INSTALL_CONVOP (octave_ ## tfrom, octave_uint8_matrix, tfrom ## _to_uint8) \
  INSTALL_CONVOP (octave_ ## tfrom, octave_uint16_matrix, tfrom ## _to_uint16) \
  INSTALL_CONVOP (octave_ ## tfrom, octave_uint32_matrix, tfrom ## _to_uint32) \
  INSTALL_CONVOP (octave_ ## tfrom, octave_uint64_matrix, tfrom ## _to_uint64)

void
install_int_conv_ops (void)
{
  INSTALL_CONVOPS (scalar)
  INSTALL_CONVOPS (matrix)
  INSTALL_CONVOPS (bool)
  INSTALL_CONVOPS (bool_matrix)
  INSTALL_CONVOPS (range)
  INSTALL_CONVOPS (char_matrix_sq_str)
  INSTALL_CONVOPS (char_matrix_dq_str)

  INSTALL_INT_CONV_FUNCTIONS (int8)
  INSTALL_INT_CONV_FUNCTIONS (int16)
  INSTALL_INT_CONV_FUNCTIONS (int32)
  INSTALL_INT_CONV_FUNCTIONS (int64)

  INSTALL_INT_CONV_FUNCTIONS (uint8)
  INSTALL_INT_CONV_FUNCTIONS (uint16)
  INSTALL_INT_CONV_FUNCTIONS (uint32)
  INSTALL_INT_CONV_FUNCTIONS (uint64)
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
