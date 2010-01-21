/*

Copyright (C) 2004, 2005, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <climits>

#include <iostream>

#include "lo-ieee.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "quit.h"

#include "defun.h"
#include "gripes.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ops.h"
#include "ov-base.h"

#ifdef HAVE_HDF5
#define HDF5_SAVE_TYPE H5T_NATIVE_UINT64
#endif

#include "ov-base-int.h"
#include "ov-base-int.cc"
#include "ov-uint64.h"
#include "ov-type-conv.h"
#include "pr-output.h"
#include "variables.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-utils.h"
#include "ls-hdf5.h"

template class octave_base_matrix<uint64NDArray>;

template class octave_base_int_matrix<uint64NDArray>;

DEFINE_OCTAVE_ALLOCATOR (octave_uint64_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_uint64_matrix,
				     "uint64 matrix", "uint64");

template class octave_base_scalar<octave_uint64>;

template class octave_base_int_scalar<octave_uint64>;

DEFINE_OCTAVE_ALLOCATOR (octave_uint64_scalar);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_uint64_scalar,
				     "uint64 scalar", "uint64");

DEFUN (uint64, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} uint64 (@var{x})\n\
Convert @var{x} to unsigned 64-bit integer type.\n\
@end deftypefn")
{
  OCTAVE_TYPE_CONV_BODY (uint64);
}
