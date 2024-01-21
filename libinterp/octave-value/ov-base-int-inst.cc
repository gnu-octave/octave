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

#include "int8NDArray.h"
#include "int16NDArray.h"
#include "int32NDArray.h"
#include "int64NDArray.h"
#include "uint8NDArray.h"
#include "uint16NDArray.h"
#include "uint32NDArray.h"
#include "uint64NDArray.h"

#include "ov-base-int.cc"

// instantiate template class with types that need to be exported from library

template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_matrix<int8NDArray>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_matrix<int16NDArray>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_matrix<int32NDArray>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_matrix<int64NDArray>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_matrix<uint8NDArray>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_matrix<uint16NDArray>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_matrix<uint32NDArray>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_matrix<uint64NDArray>;

template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_scalar<octave_int8>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_scalar<octave_int16>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_scalar<octave_int32>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_scalar<octave_int64>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_scalar<octave_uint8>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_scalar<octave_uint16>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_scalar<octave_uint32>;
template class OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API octave_base_int_scalar<octave_uint64>;

