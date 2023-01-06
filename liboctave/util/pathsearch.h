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

#if ! defined (octave_pathsearch_h)
#define octave_pathsearch_h 1

#include "octave-config.h"

#include <list>
#include <string>

OCTAVE_BEGIN_NAMESPACE(octave)

class
OCTAVE_API
directory_path
{
public:

  directory_path (const std::string& s = "");

  directory_path (const directory_path&) = default;

  directory_path& operator = (const directory_path&) = default;

  ~directory_path (void) = default;

  void set (const std::string& s)
  {
    m_initialized = false;
    m_orig_path = s;
    init ();
  }

  std::list<std::string> elements (void);

  std::list<std::string> all_directories (void);

  std::string find_first (const std::string&);

  std::string find (const std::string& nm) { return find_first (nm); }

  std::list<std::string> find_all (const std::string&);

  std::string find_first_of (const std::list<std::string>& names);

  std::list<std::string>
  find_all_first_of (const std::list<std::string>& names);

  void rehash (void)
  {
    m_initialized = false;
    init ();
  }

  static char path_sep_char (void);

  // static void path_sep_char (char c);

  static std::string path_sep_str (void);

  static bool is_path_sep (char c) { return c == path_sep_char (); }

private:

  // The colon separated list that we were given.
  std::string m_orig_path;

  // TRUE means we've unpacked the path p.
  bool m_initialized;

  // A version of the colon separate list on which we have performed
  // tilde, variable, and possibly default path expansion.
  std::string m_expanded_path;

  // The elements of the list.
  std::list<std::string> m_path_elements;

  void init (void);
};

OCTAVE_END_NAMESPACE(octave)

#endif
