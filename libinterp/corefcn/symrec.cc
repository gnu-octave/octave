////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#include <sstream>

#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"

#include "fcn-info.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "symrec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

std::shared_ptr<symbol_record::symbol_record_rep>
symbol_record::symbol_record_rep::dup (void) const
{
  return std::shared_ptr<symbol_record::symbol_record_rep> (new symbol_record_rep (*this));
}

octave_value
symbol_record::symbol_record_rep::dump (void) const
{
  std::map<std::string, octave_value> m
  = {{ "frame_offset", m_frame_offset },
    { "data_offset", m_data_offset },
    { "name", m_name },
    { "local", is_local () },
    { "formal", is_formal () }
  };

  return octave_value (m);
}

OCTAVE_END_NAMESPACE(octave)
