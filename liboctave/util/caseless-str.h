////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2024 The Octave Project Developers
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

#if ! defined (octave_caseless_str_h)
#define octave_caseless_str_h 1

#include "octave-config.h"

#include <cctype>
#include <string>

class caseless_str : public std::string
{
public:

  typedef std::string::iterator iterator;
  typedef std::string::const_iterator const_iterator;

  caseless_str () = default;

  caseless_str (const std::string& s) : std::string (s) { }
  caseless_str (const char *s) : std::string (s) { }

  caseless_str (const caseless_str&) = default;

  caseless_str& operator = (const caseless_str&) = default;

  ~caseless_str () = default;

  bool operator < (const std::string& s) const
  {
    auto p1 = begin ();
    auto p2 = s.begin ();

    while (p1 != end () && p2 != s.end ())
      {
        char lp1 = std::tolower (*p1);
        char lp2 = std::tolower (*p2);

        if (lp1 > lp2)
          return false;
        if (lp1 < lp2)
          return true;

        p1++;
        p2++;
      }

    if (length () >= s.length ())
      return false;
    else
      return true;
  }

  // Case-insensitive comparison.
  bool compare (const std::string& s, std::size_t limit = std::string::npos) const
  {
    auto p1 = begin ();
    auto p2 = s.begin ();

    std::size_t k = 0;

    while (p1 != end () && p2 != s.end () && k++ < limit)
      {
        if (std::tolower (*p1) != std::tolower (*p2))
          return false;

        p1++;
        p2++;
      }

    return (limit == std::string::npos) ? size () == s.size () : k == limit;
  }
};

#endif
