////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024 The Octave Project Developers
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

#include "ov-base-scalar.cc"

// instantiate template class with types that need to be exported from library

template class OCTINTERP_API octave_base_scalar<bool>;
template class OCTINTERP_API octave_base_scalar<double>;
template class OCTINTERP_API octave_base_scalar<float>;
template class OCTINTERP_API octave_base_scalar<Complex>;
template class OCTINTERP_API octave_base_scalar<FloatComplex>;
template class OCTINTERP_API octave_base_scalar<octave_int8>;
template class OCTINTERP_API octave_base_scalar<octave_int16>;
template class OCTINTERP_API octave_base_scalar<octave_int32>;
template class OCTINTERP_API octave_base_scalar<octave_int64>;
template class OCTINTERP_API octave_base_scalar<octave_uint8>;
template class OCTINTERP_API octave_base_scalar<octave_uint16>;
template class OCTINTERP_API octave_base_scalar<octave_uint32>;
template class OCTINTERP_API octave_base_scalar<octave_uint64>;
