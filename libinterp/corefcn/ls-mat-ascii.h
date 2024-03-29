////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2024 The Octave Project Developers
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

#if ! defined (octave_ls_mat_ascii_h)
#define octave_ls_mat_ascii_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

class octave_value;

extern OCTINTERP_API std::string
read_mat_ascii_data (std::istream& is, const std::string& filename,
                     octave_value& tc);

extern OCTINTERP_API bool
save_mat_ascii_data (std::ostream& os, const octave_value& val_arg,
                     int precision, bool tabs = false);

extern OCTINTERP_API bool
looks_like_mat_ascii_file (std::istream& is, const std::string& filename);

#endif
