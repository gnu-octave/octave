/*

Copyright (C) 2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

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
    : path (p), b (0), e (0), len (path.length ()) { set_end (); }

  kpse_path_iterator (const kpse_path_iterator& pi)
    : path (pi.path), b (pi.b), e (pi.e), len (pi.len) { }

  kpse_path_iterator operator ++ (int)
  {
    kpse_path_iterator retval (*this);
    next ();
    return retval;
  }

  std::string operator * (void) { return path.substr (b, e-b); }

  bool operator != (const size_t sz) { return b != sz; }

private:

  const std::string& path;
  size_t b;
  size_t e;
  size_t len;

  void set_end (void);
  void next (void);

  // No assignment.
  kpse_path_iterator& operator = (const kpse_path_iterator&);
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
kpse_expand_default (const std::string& path, const std::string& fallback);

extern std::string
kpse_path_expand (const std::string& path);

extern std::string
kpse_path_find_first_of (const std::string& path,
                         const std::list<std::string>& names,
                         bool must_exist);

extern std::string
kpse_path_search (const std::string& path, const std::string& name,
                  bool must_exist);

#endif
