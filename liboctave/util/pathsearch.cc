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

#include <cstdlib>

#include <string>

#include "kpse.h"
#include "lo-error.h"
#include "lo-utils.h"
#include "oct-env.h"
#include "pathsearch.h"

OCTAVE_BEGIN_NAMESPACE(octave)

directory_path::directory_path (const std::string& s)
  : m_orig_path (s), m_initialized (false), m_expanded_path (),
    m_path_elements ()
{
  if (! m_orig_path.empty ())
    init ();
}

std::list<std::string> directory_path::elements (void)
{
  return m_initialized ? m_path_elements : std::list<std::string> ();
}

std::list<std::string> directory_path::all_directories (void)
{
  std::list<std::string> retval;

  if (m_initialized)
    {
      for (const auto& elt : m_path_elements)
        {
          std::string elt_dir = kpse_element_dir (elt);

          if (! elt_dir.empty ())
            retval.push_back (elt_dir);
        }
    }

  return retval;
}

std::string directory_path::find_first (const std::string& nm)
{
  return m_initialized ? kpse_path_search (m_expanded_path, nm) : "";
}

std::list<std::string> directory_path::find_all (const std::string& nm)
{
  return (m_initialized
          ? kpse_all_path_search (m_expanded_path, nm)
          : std::list<std::string> ());
}

std::string
directory_path::find_first_of (const std::list<std::string>& names)
{
  return (m_initialized
          ? kpse_path_find_first_of (m_expanded_path, names) : "");
}

std::list<std::string>
directory_path::find_all_first_of (const std::list<std::string>& names)
{
  return (m_initialized
          ? kpse_all_path_find_first_of (m_expanded_path, names)
          : std::list<std::string> ());
}

void directory_path::init (void)
{
  static bool octave_kpse_initialized = false;

  if (! octave_kpse_initialized)
    {
      std::string val = sys::env::getenv ("KPATHSEA_DEBUG");

      if (! val.empty ())
        kpse_debug |= atoi (val.c_str ());

      octave_kpse_initialized = true;
    }

  m_expanded_path = kpse_path_expand (m_orig_path);

  for (kpse_path_iterator pi (m_expanded_path); pi != std::string::npos; pi++)
    m_path_elements.push_back (*pi);

  m_initialized = true;
}

char directory_path::path_sep_char (void)
{
  return SEPCHAR;
}

std::string directory_path::path_sep_str (void)
{
  return SEPCHAR_STR;
}

OCTAVE_END_NAMESPACE(octave)
