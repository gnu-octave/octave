////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

// Instantiate Arrays of octave_values.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "Array-oct.cc"

#include "ov.h"
#include "cdef-class.h"
#include "cdef-object.h"

#include "oct-sort.cc"

#if defined (HAVE_PRAGMA_GCC_VISIBILITY)
// Visibility attributes are ignored on template instantiation.
// As a work-around, set visibility to default overriding compiler options.
#  pragma GCC visibility push(default)
#endif

NO_INSTANTIATE_ARRAY_SORT_API (octave_value, OCTINTERP_API);
INSTANTIATE_ARRAY (octave_value, OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API);

NO_INSTANTIATE_ARRAY_SORT_API (octave_value *, OCTINTERP_API);
INSTANTIATE_ARRAY (octave_value *, OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API);

NO_INSTANTIATE_ARRAY_SORT_API (octave::cdef_object, OCTINTERP_API);
INSTANTIATE_ARRAY (octave::cdef_object, OCTINTERP_CLASS_TEMPLATE_INSTANTIATION_API);

#if defined (HAVE_PRAGMA_GCC_VISIBILITY)
#  pragma GCC visibility pop
#endif

