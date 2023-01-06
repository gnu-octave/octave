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

#if ! defined (octave_glob_match_h)
#define octave_glob_match_h 1

#include "octave-config.h"

#include <string>

#include "Array.h"
#include "str-vec.h"

class
OCTAVE_API
glob_match
{
public:

  enum opts
  {
    pathname = 1,  // No wildcard can ever match '/'.
    noescape = 2,  // Backslashes don't quote special chars.
    period = 4     // Leading '.' is matched only explicitly.
  };

  glob_match (const std::string& p,
              unsigned int xopts = pathname | noescape | period)
    : m_pat (p), m_fnmatch_flags (opts_to_fnmatch_flags (xopts))
  { }

  glob_match (const string_vector& p = string_vector (),
              unsigned int xopts = pathname | noescape | period)
    : m_pat (p), m_fnmatch_flags (opts_to_fnmatch_flags (xopts))
  { }

  glob_match (const glob_match& gm) = default;

  glob_match& operator = (const glob_match& gm) = default;

  ~glob_match (void) = default;

  void set_pattern (const std::string& p) { m_pat = p; }

  void set_pattern (const string_vector& p) { m_pat = p; }

  bool match (const std::string& str) const;

  Array<bool> match (const string_vector& str) const
  {
    int n = str.numel ();

    Array<bool> retval (dim_vector (n, 1));

    for (int i = 0; i < n; i++)
      retval(i) = match (str[i]);

    return retval;
  }

  // We forward to glob_internal here to avoid problems with gnulib's
  // glob.h defining glob to be rpl_glob.

  string_vector glob (void) const;

private:

  // Globbing pattern(s).
  string_vector m_pat;

  // Option flags.
  int m_fnmatch_flags;

  int opts_to_fnmatch_flags (unsigned int xopts) const;
};

#endif
