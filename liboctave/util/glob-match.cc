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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "glob-match.h"
#include "oct-glob.h"
#include "glob-wrappers.h"

bool
glob_match::match (const std::string& str) const
{
  return octave::sys::fnmatch (m_pat, str, m_fnmatch_flags);
}

string_vector
glob_match::glob (void) const
{
  return octave::sys::glob (m_pat);
}

int
glob_match::opts_to_fnmatch_flags (unsigned int xopts) const
{
  int retval = 0;

  if (xopts & pathname)
    retval |= octave_fnm_pathname_wrapper ();

  if (xopts & noescape)
    retval |= octave_fnm_noescape_wrapper ();

  if (xopts & period)
    retval |= octave_fnm_period_wrapper ();

  return retval;
}
