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
glob_match::glob () const
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

symbol_match::symbol_match (const std::string& pattern)
{
  m_pat = pattern;

#if defined (OCTAVE_USE_WINDOWS_API)
  m_glob = nullptr;
#else
  m_glob = std::unique_ptr<glob_match> {new glob_match {pattern}};
#endif
}

bool
symbol_match::match (const std::string& sym)
{
#if defined (OCTAVE_USE_WINDOWS_API)

  // gnulib's fnmatch replacement is slow on Windows.
  // We don't need full POSIX compatibility to match symbol patterns.
  // Glob patterns with '*' or '?' should be good enough.
  // We also do not need to worry about multi-byte characters because symbols
  // are ASCII-only.
  octave_idx_type pat_len = m_pat.length ();
  octave_idx_type pat_idx = 0;
  octave_idx_type pat_wildc_idx = -1;
  octave_idx_type sym_len = sym.length ();
  octave_idx_type sym_idx = 0;
  octave_idx_type sym_wildc_idx;

  while (sym_idx < sym_len)
    {
      if (pat_idx < pat_len
          && (m_pat[pat_idx] == '?' || m_pat[pat_idx] == sym[sym_idx]))
        {
          // match to '?' or exact match
          pat_idx++;
          sym_idx++;
        }
      else if (pat_idx < pat_len && m_pat[pat_idx] == '*')
        {
          // remember position in pattern and symbol
          pat_wildc_idx = pat_idx;
          sym_wildc_idx = sym_idx;
          pat_idx++;
        }
      else if (pat_wildc_idx != -1)
        {
          // no match but previous wildcard '*'
          // revert pat_idx to previous position
          pat_idx = pat_wildc_idx + 1;
          // but proceed to next character in symbol and try to match again
          sym_wildc_idx++;
          sym_idx = sym_wildc_idx;
        }
      else
        // no exact match and no wildcard
        return false;
    }

  // consume potentially trailing '*' in pattern
  while (pat_idx < pat_len && m_pat[pat_idx] == '*')
    pat_idx++;

  // check for remaining (unmatched) characters in pattern
  return pat_idx == pat_len;

#else

  return m_glob->match (sym);

#endif
}

