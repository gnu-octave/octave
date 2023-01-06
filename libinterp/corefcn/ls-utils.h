////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2023 The Octave Project Developers
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

#if ! defined (octave_ls_utils_h)
#define octave_ls_utils_h 1

#include "octave-config.h"

#include "data-conv.h"

OCTAVE_BEGIN_NAMESPACE(octave)

extern OCTINTERP_API save_type
get_save_type (double max_val, double min_val);

extern OCTINTERP_API save_type
get_save_type (float max_val, float min_val);

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::get_save_type' instead")
inline save_type
get_save_type (double max_val, double min_val)
{
  return octave::get_save_type (max_val, min_val);
}

OCTAVE_DEPRECATED (7, "use 'octave::get_save_type' instead")
inline save_type
get_save_type (float max_val, float min_val)
{
  return octave::get_save_type (max_val, min_val);
}

#endif

#endif
