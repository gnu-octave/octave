////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2004-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "lo-ieee.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "quit.h"

#include "errwarn.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "oct-hdf5.h"
#include "ops.h"
#include "ov-base.h"

#if defined (HAVE_HDF5)
#  define HDF5_SAVE_TYPE H5T_NATIVE_UINT16
#else
// This value will not be used.
#  define HDF5_SAVE_TYPE 0
#endif

#include "ov-base-int.h"
#include "ov-base-int.cc"
#include "ov-uint16.h"
#include "pr-output.h"
#include "variables.h"

#include "byte-swap.h"
#include "ls-oct-text.h"
#include "ls-utils.h"
#include "ls-hdf5.h"

octave_hdf5_id octave_uint16_matrix::s_hdf5_save_type = HDF5_SAVE_TYPE;
octave_hdf5_id octave_uint16_scalar::s_hdf5_save_type = HDF5_SAVE_TYPE;

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class octave_base_scalar<double>;

template class octave_base_matrix<uint16NDArray>;

template class octave_base_int_matrix<uint16NDArray>;

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_uint16_matrix,
                                     "uint16 matrix", "uint16");

template class octave_base_scalar<octave_uint16>;

template class octave_base_int_scalar<octave_uint16>;

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_uint16_scalar,
                                     "uint16 scalar", "uint16");
