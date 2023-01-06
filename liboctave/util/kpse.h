////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

#if ! defined (octave_kpse_h)
#define octave_kpse_h 1

#include "octave-config.h"

#include <list>
#include <string>

// A way to step through a path, extracting one directory name at a
// time.

class kpse_path_iterator
{
public:

  kpse_path_iterator (const std::string& p)
    : m_path (p), m_b (0), m_e (0), m_len (m_path.length ())
  {
    set_end ();
  }

  kpse_path_iterator (const kpse_path_iterator&) = default;

  // No assignment!

  kpse_path_iterator& operator = (const kpse_path_iterator&) = delete;

  ~kpse_path_iterator (void) = default;

  kpse_path_iterator operator ++ (int)
  {
    kpse_path_iterator retval (*this);
    next ();
    return retval;
  }

  std::string operator * (void) { return m_path.substr (m_b, m_e-m_b); }

  bool operator != (const std::size_t sz) { return m_b != sz; }

private:

  const std::string& m_path;
  std::size_t m_b;
  std::size_t m_e;
  std::size_t m_len;

  void set_end (void);
  void next (void);
};

extern unsigned int kpse_debug;

extern std::list<std::string>
kpse_all_path_search (const std::string& path, const std::string& name);

extern std::list<std::string>
kpse_all_path_find_first_of (const std::string& path,
                             const std::list<std::string>& names);

extern std::string
kpse_element_dir (const std::string& elt);

extern std::list<std::string>
kpse_all_path_search (const std::string& path, const std::string& name);

extern std::string
kpse_path_expand (const std::string& path);

extern std::string
kpse_path_find_first_of (const std::string& path,
                         const std::list<std::string>& names);

extern std::string
kpse_path_search (const std::string& path, const std::string& name);

#endif
