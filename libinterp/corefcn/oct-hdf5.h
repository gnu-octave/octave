////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#if ! defined (octave_oct_hdf5_h)
#define octave_oct_hdf5_h 1

#include "octave-config.h"

#if defined (HAVE_HDF5)

#  if defined (HAVE_HDF5_H)
#    include <hdf5.h>
#  endif

#  include "oct-hdf5-types.h"

#  if defined (OCTAVE_ENABLE_64)
#    define H5T_NATIVE_IDX H5T_NATIVE_INT64
#  else
#    define H5T_NATIVE_IDX H5T_NATIVE_INT
#  endif

#endif

#endif
