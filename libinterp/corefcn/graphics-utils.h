////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#if ! defined (octave_graphics_utils_h)
#define octave_graphics_utils_h 1

// The functions defined here are private and should not be exported.
// This header file should not be installed.

#include "octave-config.h"

#include <string>

#include "graphics-handle.h"

class caseless_str;
class octave_value;
class NDArray;

OCTAVE_BEGIN_NAMESPACE(octave)

// Flag to stop redraws due to callbacks while deletion is in progress.
extern bool delete_executing;

extern void xset (const graphics_handle& h, const caseless_str& pname,
                  const octave_value& val);

extern void xset (const graphics_handle& h, const octave_value_list& args);

extern octave_value xget (const graphics_handle& h, const caseless_str& pname);

extern bool isfigure (double val);

extern graphics_handle
reparent (const octave_value& ov, const std::string& who,
          const std::string& pname, const graphics_handle& new_parent,
          bool adopt = true);

extern void
delete_graphics_object (const graphics_handle& h, bool from_root = false);

extern void delete_graphics_object (double val, bool from_root = false);

extern void
delete_graphics_objects (const NDArray vals, bool from_root = false);

extern void close_figure (const graphics_handle& h);

extern void force_close_figure (const graphics_handle& h);

OCTAVE_END_NAMESPACE(octave)

#endif
