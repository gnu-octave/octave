////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2021 The Octave Project Developers
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

#if defined (HAVE_LLVM)

#include "Array.h"
#include "Array.cc"

#include "jit-ir.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class Array<octave::idx_vector>;
extern template class Array<octave_idx_type>;

NO_INSTANTIATE_ARRAY_SORT (octave::jit_function, OCTINTERP_API);

// Visibility attributes are ignored on template instantiation.
// As a work-around, set visibility to default overriding compiler options.
#pragma GCC visibility push(default)
INSTANTIATE_ARRAY (octave::jit_function, OCTINTERP_API);
#pragma GCC visibility pop

#if defined (Cell_h)
#  error Must not include Cell.h in Array-jit.h
#  error This causes problems on MSVC
#endif

#endif
